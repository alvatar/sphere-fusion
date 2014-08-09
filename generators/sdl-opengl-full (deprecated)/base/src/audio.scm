(define select1-sound #f)

(define (init-audio)
  (if (= (Mix_OpenAudio 22050 MIX_DEFAULT_FORMAT 2 8192) -1)
      (fusion:error "Error initializing audio: " (Mix_GetError)))
  (set! select1-sound (Mix_LoadWAV "assets/sounds/select1.ogg")))

(define (play-sound)
  (Mix_PlayChannel -1 select1-sound 0))

(define (destroy-audio)
  (Mix_FreeChunk select1-sound)
  (Mix_CloseAudio))
