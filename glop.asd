;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem glop
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Morgan Veyret <patzy at oxyde dot org>"
  :depends-on (:cffi :trivial-garbage :split-sequence)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             #+(and unix (not darwin))
             (:module "x11"
                      :serial t
                      :components ((:file "package")
                                   (:file "keysymdef")
                                   (:file "xlib")
                                   (:file "xkb")
                                   (:file "xcomposite")
                                   (:file "glx")
                                   (:file "display-ctrl")
                                   (:file "glop-x11")))
             #+darwin
             (:module "osx"
                      :serial t
                      :components ((:file "package")
                                   (:file "carbon")
                                   (:file "bridge")
                                   (:file "foundation")
                                   (:file "appkit")
                                   (:file "quartz")
                                   (:file "glop-app")
                                   (:file "glop-view")
                                   (:file "glop-osx")))
             #+(or win32 windows)
             (:module "win32"
                      :serial t
                      :components ((:file "package")
                                   (:file "win32")
                                   (:file "wgl")
                                   (:file "dwm")
                                   (:file "glop-win32")))
             (:file "glop")))))


(asdf:defsystem glop/egl
  :license "MIT"
  :version "0.1.0"
  :description "Direct FFI bindings for OpenGL window and context management"
  :author "Morgan Veyret <patzy at oxyde dot org>"
  :depends-on (:cffi :trivial-garbage :split-sequence :cl-egl)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             #+(and unix (not darwin))
             (:module "x11"
                      :serial t
                      :components ((:file "package")
                                   (:file "keysymdef")
                                   (:file "xlib")
                                   (:file "xkb")
                                   (:file "xcomposite")
                                   (:file "display-ctrl")
                                   (:file "glop-x11")))
             #+(or win32 windows)
             (:module "win32"
                      :serial t
                      :components ((:file "package")
                                   (:file "win32")
                                   (:file "dwm")
                                   (:file "glop-win32")))
             (:module "egl"
                      :serial t
                      :components ((:file "egl")))
             (:file "glop")))))

