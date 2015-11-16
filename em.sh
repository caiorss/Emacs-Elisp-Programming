#!/usr/bin/env sh
#
#  Emacs Client Wrapper
# 
#  It helps to manage Emacs client and daemon.
# 
# 
#  
#
#-----------------------------------------------

BROWSER=chromium-browser

case "$1" in

    # Start Emacs Server
    -start)
        emacs --daemon ;;

    -stop)
        emacsclient --eval "(kill-emacs)" ;;
        
    -restart)
        emacsclient --eval "(kill-emacs)" ;
        emacs --daemon ;;

    # Open Emacs Frame (GUI)
    -gui)
        if [ -z "$2"] ;
        then
             emacsclient -a "" -c &
        else
             emacsclient -a "" -c "$2" &
        fi
             ;;
        
    # Open Emacs in Terminal
    -cli)
        if [ -z "$2"] ;
        then
             emacsclient -a "" -t
        else
             emacsclient -a "" -t "$2"
        fi
        ;;

    # Edit file without enter CLI or GUI interface.
    -edit)
        emacsclient -a "" -n "$2" ;;
        
    # Execute Command (Eval)
    -cmd)
        emacsclient -a "" -n --eval "$2" ;;

    # Load Emacs Lisp Source
    -load)
        emacsclient -a "" -n --eval "(load \"$2\")" ;;

    -man)
        emacsclient -a "" -t -e "(woman \"$2\")"  ;;

    -manual)
        $BROWSER "http://www.gnu.org/software/emacs/manual" ;;

    -show)

        case "$2" in
             load-path)
                 emacsclient -a ""   --eval "load-path" \
                              | sed 's/["|\)|\(]//g' | sed 's/\s/\n/g'
                 ;;

            exec-path)
                emacsclient -a ""   --eval "exec-path" \
                    | sed 's/["|\)|\(]//g' | sed 's/\s/\n/g'
                ;;
        esac
        ;;

     ## List all files opened. 
     ## 
        -files)

            emacsclient -a ""   --eval "(remove-if #'null (mapcar #'buffer-file-name (buffer-list)))" \
                        | sed 's/["|\)|\(]//g' | sed 's/\s/\n/g'
            ;;

      ## Open Emacs shell in a new window
       # -ielm)
       #     emacsclient -a "" -c  --eval "(progn (new-frame) (ielm))" 
       #     ;;

      ## Open Emascs Elisp shell (IELM) on terminal:
     -ielm)
         emacsclient -a "" -t --eval "(ielm)"
         ;;

     ## Open Emacs eshell on terminal
     -eshell)
         emacsclient -a "" -t --eval "(eshell)"
         ;;
     
    *)

echo "
Emacs Sever/Client Manager (em)

Usage:

./em <option>

 -start           Starts Emacs as sever, daemon.

 -stop            Stop Emacs daemon.

-edit <file>      Open file in daemon.

 -gui             Open a new Emacs frame

 -cli             Open Emacs in terminal

 -cmd <command>   Evaluates s-expression in Emacs, example:
                  ./em.sh \"(+ 1 2 3 4 5)\"

 -files           Show all files opened.
 
 -load <file.el>  Load a Emacs Lisp source file.

 -man <manpage>    Browser man page in terminal, example:
                  ./em -man cbrt

 -manual          Open Emacs Manual in Web Browser:

 ------------------------------------------------
 -show [option]   Get Emacs Information.

       load-path  Show Emacs Load Path
       exec-path  Show Emacs Exec Path

 
"

    ;;
esac
