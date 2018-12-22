(in-package #:glop)
#++
(ql:quickload '(glop glop/egl cl-opengl/es2))

(let ((p (find-package :glop-wgl)))
  (when (and (print p)
             (print (find-symbol "OPENGL" p))
             (print (ignore-errors (cffi:foreign-library-loaded-p
                              (find-symbol "OPENGL" p)))))
    (cerror "load egl version"
            "can't use glop/egl and normal glop at same time")
    (cffi:close-foreign-library (find-symbol "OPENGL" p))))

(let ((p (find-package :glop-glx)))
  (when (and (print p)
             (print (find-symbol "OPENGL" p))
             (print (ignore-errors (cffi:foreign-library-loaded-p
                              (find-symbol "OPENGL" p)))))
    (cerror "load egl version"
            "can't use glop/egl and normal glop at same time")
    (cffi:close-foreign-library (find-symbol "OPENGL" p))))

(defclass egl-context-mixin ()
  ((egl-display :accessor egl-display :initform nil)
   (egl-surface :accessor egl-surface :initform nil)
   (egl-config :accessor egl-config :initform nil)
   (egl-double-buffer :accessor egl-double-buffer :initform t)))

(defstruct egl-context
  dpy win ctx)

#++
(defmethod egl-display (w)
  (win32-window-dc w))
#++
(defmethod egl-window (w)
  (win32-window-id w))
#++
(defmethod egl-pixmap (w)
  (win32-window-id w))


(defmethod window-created-hook ((w egl-context-mixin))
  (setf (egl-display w)
        (egl:get-display #+win32 (win32-window-dc w)
                         #-win32 (x11-window-display w)))
  (multiple-value-bind (maj min) (egl:initialize (egl-display w))
    (format t "egl initialized, version ~s.~s~%" maj min))
  (egl:bind-api :opengl-es-api)
  (format t "egl api ~s~%" (egl:query-api))
  (loop for i in '(:client-apis :vendor :version :extensions)
        do (format t "~s: ~s~%"
                   i (egl:query-string (egl-display w) i))))


;; used by win32 code
(defmethod choose-pixel-format ((w egl-context-mixin)
                                &key rgba
                                  (double-buffer t)
                                  stereo
                                  (red-size 0)
                                  (green-size 0)
                                  (blue-size 0)
                                  (alpha-size 0)
                                  (depth-size 0)
                                  accum-buffer
                                  (accum-red-size 0)
                                  (accum-green-size 0)
                                  (accum-blue-size 0)
                                  stencil-buffer (stencil-size 0))
  (declare (ignore stereo accum-buffer
                   accum-red-size accum-green-size accum-blue-size))
  ;; not completely sure about the interpretation of :rgba for egl,
  ;; since it has color key, but alpha transparency is poorly
  ;; specified.  as far as i can tell, requesting alpha bits might get
  ;; you a transparent rgba buffer, so mapping it to that for now
  (let ((configs (egl::choose-config*
                  (egl-display w)
                  :color-buffer-type :rgb-buffer
                  :renderable-type :opengl-es2-bit
                  :red-size red-size
                  :green-size green-size
                  :blue-size blue-size
                  :alpha-size (if rgba (max 1 alpha-size) 0)
                  :depth-size depth-size
                  :stencil-size (if stencil-buffer stencil-size 0)
                  :none)))
    ;; set when creating surface in create-gl-context
    (setf (egl-double-buffer w) double-buffer)
    #++
    (format t "~& ~d configs =~{~%   ~s~}~%"
            (length configs)
            (loop for i in configs
                  collect (egl::get-config-attribs (egl-display w) i)))
    (let ((c (if rgba
                 (first configs)
                 (find-if (lambda (a)
                         (zerop (egl:get-config-attrib (egl-display w) a
                                                       :alpha-size)))
                       configs))))
      (format t "~&egl selected config ~s~%"
              (egl:get-config-attribs (egl-display w) c))
      (setf (egl-config w) c))))

;; used by x11 code
(defmethod choose-visual ((w egl-context-mixin) attribs)
  (apply #'choose-pixel-format w attribs)
  (let* ((config (egl-config w))
         (visual (egl:get-config-attrib (egl-display w)
                                        config :native-visual-id)))
    (cffi:with-foreign-objects ((num-visuals :int)
                           (vi '(:struct glop-xlib::visual-info)))
      (setf (cffi:foreign-slot-value vi '(:struct glop-xlib::visual-info)
                                     'glop-xlib::visual-id)
            visual)
      (setf (x11-window-visual-infos w)
            (glop-xlib::get-visual-info (x11-window-display w)
                                        :id vi num-visuals))
      (format t "got visual: ~s~%"
              (cffi:mem-ref (x11-window-visual-infos w)
                            '(:struct glop-xlib::visual-info))))))

(defmethod create-gl-context ((w egl-context-mixin)
                              &key (make-current t)
                                major minor
                                forward-compat debug
                                profile
                                share)
  ;; todo: add these to create-context
  (declare (ignore forward-compat debug profile)
           (ignorable minor))
  (setf (egl-surface w) (egl:create-window-surface
                         (egl-display w)
                         (egl-config w)
                         #+win32 (win32-window-id w)
                         #-win32 (x11-window-id w)
                         :render-buffer (if (egl-double-buffer w)
                                            :back-buffer
                                            :single-buffer)))
  (setf (window-gl-context w)
        (make-egl-context
         :win nil
         :dpy (egl-display w)
         :ctx (egl:create-context
               (egl-display w)
               (egl-config w)
               (or share :no-context)
               ;; fixme: fix choose-pixel-format to handle es1
               :context-major-version (or major 2)
               #+:context-minor-version (or minor 0)
               :none)))
  (when make-current
    (attach-gl-context w (window-gl-context w)))
  (format t "got egl version ~s~%"
          (egl::check (egl:query-string (egl-display w) :version)))
  #++(when (and major minor)
    (multiple-value-bind (actual-major actual-minor)
        (glop::parse-gl-version-string-values
         (glop-wgl::get-string :version))
      (when (or (< actual-major major)
                (and (= actual-major major) (< actual-minor minor)))
        (error "unable to create requested context")))))


(defmethod destroy-gl-context ((ctx egl-context))
  (detach-gl-context ctx)
  (egl:destroy-context (egl-context-dpy ctx) (egl-context-ctx ctx)))

(defmethod attach-gl-context ((w egl-context-mixin) (ctx egl-context))
  (when (egl-context-win ctx)
    (detach-gl-context ctx))
  (setf (window-gl-context w) ctx)
  (setf (egl-context-win ctx) w)
  (egl::check
   (egl:make-current (egl-display w)
                     (egl-surface w)
                     (egl-surface w)
                     (egl-context-ctx (window-gl-context w)))))

(defmethod detach-gl-context ((ctx egl-context))
  (let ((w (egl-context-win ctx)))
    (setf (egl-context-win ctx) nil)
    (when w
      (egl::check
       (egl:make-current (egl-display w)
                         :no-surface
                         :no-surface
                         :no-context)))))

(defmethod %swap-interval ((w egl-context-mixin) interval)
  (egl:swap-interval (egl-display w) interval))

(defmethod swap-buffers ((w egl-context-mixin))
  (egl:swap-buffers (egl-display w) (egl-surface w)))



#++
(let ((display (get-display :default-display)))
 (with-display (:display display :major-var maj :minor-var min)
   (format t "got egl ~s . ~s~%" maj min)
   (format t "api ~s~%" (query-api))
   (loop for i in '(:client-apis :vendor :version :extensions)
         do (format t "~s: ~s~%"
                    i (query-string display i)))))

#++
(locally (declare (notinline gl:get-string))
  (glop:with-window (w "egl?" 256 256 :win-class 'glop::egl-window
                       :alpha-size 0
                      )
   (format t "got gles version ~s~%"
           (gl::get-string :version))
   (format t "got gles vendor ~s~%"
           (gl::get-string :vendor))
   (format t "got gles renderer ~s~%"
           (gl::get-string :renderer))
   (format t "got gles glsl ~s~%"
           (gl::get-string :shading-language-version))
   (format t "got gles ext ~s~%"
           (gl::get-string :extensions))
   (loop repeat 20
         do (glop:next-event w)
            (gl:clear-color (random 1.0) (random 1.0) (random 1.0) 1)
            (gl:clear :color-buffer)
            (glop:swap-buffers w)
            (sleep 0.1))
   (sleep 2)))



