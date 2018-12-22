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

(defclass egl-context-mixin ()
  ((egl-display :accessor egl-display :initform nil)
   (egl-surface :accessor egl-surface :initform nil)
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
  (setf (egl-display w) (egl:get-display (win32-window-dc w)))
  (multiple-value-bind (maj min) (egl:initialize (egl-display w))
    (format t "egl initialized, version ~s.~s~%" maj min))
  (egl:bind-api :opengl-es-api)
  (format t "egl api ~s~%" (egl:query-api))
  (loop for i in '(:client-apis :vendor :version :extensions)
        do (format t "~s: ~s~%"
                   i (egl:query-string (egl-display w) i))))



(defmethod choose-pixel-format ((w egl-context-mixin)
                                &key (rgba t)
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
    (format t "configs =~{~%   ~s~}~%"
            (loop for i in configs
                  collect (egl::get-config-attribs (egl-display w) i)))
    (format t "egl selected config ~s~%"
            (egl:get-config-attribs (egl-display w) (first configs)))
    (first configs)))

#++
(defun get-version ()
  (glop::parse-gl-version-string-values
   (get-string :version)))

(defmethod create-gl-context ((w egl-context-mixin)
                              &key (make-current t)
                                major minor
                                forward-compat debug
                                profile
                                share)
  ;; todo: add these to create-context
  (declare (ignore forward-compat debug profile))
  (format t "create context ...~%")
  (setf (egl-surface w) (egl:create-window-surface
                         (egl-display w)
                         (win32-window-pixel-format w)
                         (win32-window-id w)
                         :render-buffer (if (egl-double-buffer w)
                                            :back-buffer
                                            :single-buffer)))
  (format t "surface = ~s~%" (egl-surface w))
  (setf (window-gl-context w)
        (make-egl-context
         :win nil
         :dpy (egl-display w)
         :ctx (egl:create-context
               (egl-display w)
               (win32-window-pixel-format w)
               (or share :no-context)
               ;; fixme: fix choose-pixel-format to handle es1
               :context-major-version (or major 2)
               :none
               #+:context-minor-version (or minor 0))))
  (format t "context = ~s (~s)~%" (window-gl-context w) make-current)
  (when make-current
    (attach-gl-context w (window-gl-context w)))
  (format t "got egl version ~s~%" (egl::check
                                    (egl:query-string (egl-display w) :version)))
  #++(format t "got gles version ~s (requested ~s.~s)~%~%"
          (glop-wgl::get-string :version) major minor)
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
  (format t "make current ~s ~s ~s ~s~%"
          (egl-display w)
          (egl-surface w)
          (egl-surface w)
          (egl-context-ctx (window-gl-context w)))
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
(glop:with-window (w "egl?" 256 256 :x 1920 :y 32 :win-class 'glop::egl-window)
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
  (sleep 2))



