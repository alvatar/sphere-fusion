(%include base: ffi#)
(%include sdl2: sdl2#)

(define (critical-error . msgs)
  (apply SDL_LogError msgs)
  (exit 1))

(define (handle-event event k)
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
	       (k))
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
      (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "FINGER!")))))

;; Quick and dirty pixel filling test
;; TODO: Test also with pure Scheme code
(define test-surface-random-pixels
  (c-lambda (SDL_Surface*) void
            "
srand(time(NULL));
// Enable RLE for faster pixel access
SDL_SetSurfaceRLE(___arg1, 1);
SDL_LockSurface(___arg1);
int i;
for (i=0; i< (___arg1->w * ___arg1->h); i++) {
  *((Uint32*)(___arg1->pixels + (i*4))) = rand();
}
SDL_UnlockSurface(___arg1);
//SDL_FillRect(___arg1, NULL, rand());
"))

(define (main)
  (if (< (SDL_Init SDL_INIT_VIDEO) 0)
      (critical-error "Couldn't initialize SDL!"))
  (let ((screen-width 512)
        (screen-height 512))
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
      (let* ((event (make-SDL_Event))
             (event* (SDL_Event-pointer event)))
        (call/cc
         (lambda (k)
           (let main-loop ()
             (let event-loop ()
               (if (= (SDL_PollEvent event*) 1)
                   (begin (handle-event event k)
                          (event-loop))))
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "LOOPING...")
             (test-surface-random-pixels (SDL_GetWindowSurface win))
             (if (not (= 0 (SDL_UpdateWindowSurface win)))
                 (SDL_LogError SDL_LOG_CATEGORY_APPLICATION (SDL_GetError)))
             (SDL_Delay 1000)
             (main-loop))))
        (SDL_DestroyWindow win)
        (SDL_Quit)))))
