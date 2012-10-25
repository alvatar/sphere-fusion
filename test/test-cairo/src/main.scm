;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo on SDL Surface

(%include base: ffi#)
(%include sdl2: sdl2#)

(define (critical-error . msgs)
  (apply SDL_LogError msgs)
  (exit 1))

(define (create-cairo-surface buffer width height channels)
  (let ((cairo-surface
         (cairo_image_surface_create_for_data
          (void*->unsigned-char* buffer)
          CAIRO_FORMAT_ARGB32
          width
          height
          (* channels width))))
    (unless (equal? (cairo_surface_status cairo-surface) CAIRO_STATUS_SUCCESS)
            (free buffer)
            (error "Couldn't create cairo surface"))
    (let ((cairo (cairo_create cairo-surface)))
      (unless (equal? (cairo_status cairo) CAIRO_STATUS_SUCCESS)
              (free buffer)
              (error "Couldn't create context"))
      (make-will cairo
                 (lambda (c)
                   (free buffer)
                   (cairo_destroy c)))
      (values cairo cairo-surface buffer))))

(define (handle-event event)
  (let ((type (SDL_Event-type event)))
    (cond
     ((= type SDL_QUIT)
      'exit)
     ((= type SDL_MOUSEBUTTONDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Button down"))
     ((= type SDL_KEYDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Key down")
      (let* ((kevt (SDL_Event-key event))
	     (key (SDL_Keysym-sym
		   (SDL_KeyboardEvent-keysym kevt))))
	(cond ((= key SDLK_ESCAPE)
	       'exit)
              (else
	       (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
     ((= type SDL_WINDOWEVENT)
      (let* ((wevt (SDL_Event-window event))
             (event (SDL_WindowEvent-event wevt)))
        (cond
         ((= event SDL_WINDOWEVENT_SIZE_CHANGED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Size Changed")
          'size-changed)
         ((= event SDL_WINDOWEVENT_RESIZED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Resized")
          'resized)
         ((= event SDL_WINDOWEVENT_MINIMIZED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Minimized"))
         ((= event SDL_WINDOWEVENT_RESTORED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Restored")))))
     ((= type SDL_FINGERDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "FINGER DOWN!")))))

(define draw
  (let ((posx 180.0))
    (lambda (cr)
      (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
      (cairo_rectangle cr 0.0 0.0 (exact->inexact 1280) (exact->inexact 727))
      (cairo_fill cr)
      (cairo_arc cr posx 180.0 150.0 0.0 6.28)
      (cairo_set_source_rgb cr 0.5 0.5 0.0)
      (cairo_fill cr)
      (set! posx (+ 1.0 posx)))))

(define (main)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
      (critical-error "Couldn't initialize SDL!"))
  (let ((screen-width 1280)
        (screen-height 727))
    (let* ((win (SDL_CreateWindow
                 ""
                 SDL_WINDOWPOS_CENTERED
                 SDL_WINDOWPOS_CENTERED
                 screen-width
                 screen-height
                 (cond-expand
                  (mobile (bitwise-ior SDL_WINDOW_FULLSCREEN SDL_WINDOW_BORDERLESS))
                  (else 0))))
           (check (if (not win) (critical-error "Unable to create render window" (SDL_GetError)))))
      (SDL_Log (string-append "Successfully created SDL render surface: "
                              (object->string
                               (pointer->SDL_Surface (SDL_GetWindowSurface win)))))
      (SDL_LogSetAllPriority SDL_LOG_PRIORITY_DEBUG)

      #;
      (cond-expand
      (mobile (remote-debug "192.168.1.128"))
      (else (void)))
      
      ;; FIX: Handle device rotation and surface destruction. Seems to be a SDL issue
      (receive
       (cairo cairo-surface cairo-surface-data)
       (create-cairo-surface (SDL_Surface-pixels
                              (pointer->SDL_Surface
                               (SDL_GetWindowSurface win)))
                             screen-width
                             screen-height
                             4)
       (let* ((event (make-SDL_Event))
              (event* (SDL_Event-pointer event)))
         (SDL_Log "Cairo surface successfully created")
         (SDL_Log "SDL_CreatedRGBSurface works well (apparently)")
         (call/cc
          (lambda (exit)
            (let main-loop ()
              (let event-loop ()
                (when (= (SDL_PollEvent event*) 1)
                      (case (handle-event event)
                        ((exit)
                         (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
                         (SDL_DestroyWindow win)
                         (SDL_Quit)
                         (exit))
                        ((size-changed)
                         (SDL_Log "SIZE CHANGED"))
                        ((resized)
                         (SDL_Log "RESIZED")))
                      (event-loop)))
              (draw cairo)
                                        ;(SDL_Log "Cairo draws")
              (unless (= 0 (SDL_UpdateWindowSurface win))
                      (critical-error "Unable to update Window Surface" (SDL_GetError)))
              (SDL_Delay 40)
              (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Looping...")
              (main-loop)))))))))
