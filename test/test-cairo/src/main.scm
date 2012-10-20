;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; Test for Cairo on SDL Surface

;(%include base: ffi#)
;(%include sdl2: sdl2#)

(define (critical-error . msgs)
  (apply SDL_LogError msgs)
  (exit 1))

(define (create-cairo-surface buffer width height channels)
  (let ((cairo-surface
         (cairo:image-surface-create-for-data
          (void*->unsigned-char* buffer)
          CAIRO_FORMAT_ARGB32
          width
          height
          (* channels width))))
    (unless (equal? (cairo:surface-status cairo-surface) CAIRO_STATUS_SUCCESS)
            (free buffer)
            (error "Couldn't create cairo surface"))
    (let ((cairo (cairo:create cairo-surface)))
      (unless (equal? (cairo:status cairo) CAIRO_STATUS_SUCCESS)
              (free buffer)
              (error "Couldn't create context"))
      (make-will cairo
                 (lambda (c)
                   (free buffer)
                   (cairo:destroy c)))
      (values cairo cairo-surface buffer))))

(define (draw cr)
  (cairo:arc cr 80.0 80.0 76.8 0.0 3.14)
  (cairo:clip cr)
  (cairo:new-path cr)
  (cairo:rectangle cr 0.0 0.0 256.0 256.0)
  (cairo:fill cr)
  (cairo:set-source-rgb cr 0.0 1.0 0.0)
  (cairo:move-to cr 0.0 0.0)
  (cairo:line-to cr 256.0 256.0)
  (cairo:move-to cr 256.0 0.0)
  (cairo:line-to cr 0.0 256.0)
  (cairo:set-line-width cr 10.0)
  (cairo:stroke cr)
  (cairo:restore cr))

(define (handle-event event)
  (let ((type (SDL_Event-type event)))
    (cond
     ((= type SDL_QUIT)
      (exit 0))
     ((= type SDL_MOUSEBUTTONDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Button down"))
     ((= type SDL_KEYDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Key down")
      (let* ((kevt (SDL_Event-key event))
	     (key (SDL_Keysym-sym
		   (SDL_KeyboardEvent-keysym kevt))))
	(cond ((= key SDLK_ESCAPE)
	       (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
	       (error "TODO"))
              (else
	       (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " key))))))
     ((= type SDL_WINDOWEVENT)
      (let* ((wevt (SDL_Event-window event))
             (event (SDL_WindowEvent-event wevt)))
        (cond
         ((= event SDL_WINDOWEVENT_MINIMIZED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Minimized"))
         ((= event SDL_WINDOWEVENT_RESTORED)
          (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Window Restored")))))
     ((= type SDL_FINGERDOWN)
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "FINGER DOWN!")))))


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
                 (bitwise-ior SDL_WINDOW_FULLSCREEN
                              SDL_WINDOW_BORDERLESS)))
           (check (if (not win) (critical-error "Unable to create render window"(SDL_GetError)))))
      (SDL_Log (string-append "Successfully created SDL render surface: "
                              (object->string
                               (pointer->SDL_Surface (SDL_GetWindowSurface win)))))
      (SDL_LogSetAllPriority SDL_LOG_PRIORITY_DEBUG)

      ;(remote-debug "192.168.1.128")
      
      ;; FIX: Handle device rotation and surface destruction
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
         (let main-loop ()
           (let event-loop ()
             (if (= (SDL_PollEvent event*) 1)
                 (begin (handle-event event)
                        (event-loop))))
           (draw cairo)
           (SDL_Log "Cairo draws!")
           
           (if (not (= 0 (SDL_UpdateWindowSurface win)))
               (SDL_LogError SDL_LOG_CATEGORY_APPLICATION (SDL_GetError)))
           (SDL_Delay 100)
           
           (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "LOOPING...")
           (main-loop))
         (SDL_DestroyWindow win)
         (SDL_Quit))))))
