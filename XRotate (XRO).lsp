;;; XRO - Object Rotation Alignment Tool


(defun c:XRO (/ *error* ss objData mode gr mousePt i ent obj rotPt curAng last-valid-preview 
                angle-override selection-radius closestPt param tanVec current-angle
                ent2 curveObj exact-angle p1 p2 filter-types 
                closest-ent min-dist ent-obj closest-pt dist viewAngle
                mleaderCount objCount try-result objType textEnt textData
                keyCode clickPt done line-entities success filter-list win-type
                original-objects original-angles restored)
  
  (defun *error* (msg)
    (if (or (= msg "Function cancelled") (= msg "quit / exit abort"))
      (progn
		(command-s "_.UNDO" "_BACK") ;; undo back to group start
        (princ "\nCancelled - all changes undone.")
      )
    )
    (princ)
  )
  
  ;; Settings for responsive updates
  (setq last-valid-preview nil)      ;; Track if we have a valid preview showing
  (setq angle-override nil)          ;; For manual angle override if needed
  (setq selection-radius 5)          ;; Selection radius for line detection
  (setq original-objects nil)        ;; For storing original objects
  (setq original-angles nil)         ;; For storing original angles
  (setq restored nil)                ;; Flag to track if restoration happened
  
  ;; All supported line entity types
  (setq line-entities "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,ELLIPSE,SPLINE,XLINE,MLINE,RAY")
  
  ;; Convert point to 2D safely with enhanced precision
  (defun safe-2d-point (pt / x y)
    (if (and pt (listp pt) (>= (length pt) 2))
      (progn
        (setq x (float (car pt)))
        (setq y (float (cadr pt)))
        (list x y)
      )
      nil
    )
  )
  
  ;; Optimized object point finder
  (defun get-any-point (obj / objName entName result properties objType 
                             entData code result minPoint maxPoint)
    ;; Try most common properties first based on object type for better performance
    (setq objType "UNKNOWN")
    (setq result nil)
    (if (vlax-property-available-p obj 'ObjectName)
      (setq objType (vlax-get obj 'ObjectName))
    )
    
    ;; Type-specific optimized property checking
    (cond
      ;; Text-based objects
      ((wcmatch objType "*TEXT*,*MTEXT*")
       (foreach prop '(TextPosition InsertionPoint Position)
         (if (and (not result) (vlax-property-available-p obj prop))
           (progn
             (setq try-result (vl-catch-all-apply 'vlax-get (list obj prop)))
             (if (and (not (vl-catch-all-error-p try-result)) try-result (listp try-result))
               (setq result try-result)
             )
           )
         )
       )
      )
      
      ;; Block-based objects
      ((wcmatch objType "*INSERT*,*BLOCK*")
       (foreach prop '(InsertionPoint Position BasePoint)
         (if (and (not result) (vlax-property-available-p obj prop))
           (progn
             (setq try-result (vl-catch-all-apply 'vlax-get (list obj prop)))
             (if (and (not (vl-catch-all-error-p try-result)) try-result (listp try-result))
               (setq result try-result)
             )
           )
         )
       )
      )
      
      ;; MLeader objects
      ((wcmatch objType "*MLEADER*")
       (foreach prop '(ContentPosition TextPosition LandingPoint)
         (if (and (not result) (vlax-property-available-p obj prop))
           (progn
             (setq try-result (vl-catch-all-apply 'vlax-get (list obj prop)))
             (if (and (not (vl-catch-all-error-p try-result)) try-result (listp try-result))
               (setq result try-result)
             )
           )
         )
       )
      )
      
      ;; Default case - try all properties
      (T
       (setq properties '(
         InsertionPoint Position TextPosition Center
         TextAlignmentPoint LandingPoint ContentPosition
         MidTextPoint Origin StartPoint BasePoint
       ))
       
       ;; Try common properties
       (foreach prop properties
         (if (and (not result) (vlax-property-available-p obj prop))
           (progn
             (setq try-result (vl-catch-all-apply 'vlax-get (list obj prop)))
             (if (and (not (vl-catch-all-error-p try-result)) try-result (listp try-result))
               (setq result try-result)
             )
           )
         )
       )
      )
    )
    
    ;; If we found a point, return it
    (if result
      result
      ;; Fallback to bounding box for better performance
      (progn
        (vl-catch-all-apply 'vla-getboundingbox (list obj 'minPoint 'maxPoint))
        (if (and minPoint maxPoint)
          (list 
            (/ (+ (vlax-safearray-get-element minPoint 0)
                  (vlax-safearray-get-element maxPoint 0)) 2.0)
            (/ (+ (vlax-safearray-get-element minPoint 1)
                  (vlax-safearray-get-element maxPoint 1)) 2.0)
            0.0)
          nil
        )
      )
    )
  )
  
  ;; Optimized multileader detection
  (defun is-multileader (obj / result)
    (setq result nil)
    (if (vlax-property-available-p obj 'ObjectName)
      (setq result (wcmatch (vl-catch-all-apply 'vlax-get (list obj 'ObjectName)) "*MLeader*"))
    )
    result
  )
  
  ;; Save original rotation info
  (defun save-original-rotation (obj / rotation)
    (setq rotation 0.0)
    
    ;; Get the original rotation based on object type
    (cond 
      ;; MLeader case - try both TextRotation and TextAngle
      ((is-multileader obj)
       (if (vlax-property-available-p obj 'TextRotation)
         (setq rotation (vlax-get obj 'TextRotation))
         (if (vlax-property-available-p obj 'TextAngle)
           (setq rotation (vlax-get obj 'TextAngle))
         )
       )
      )
      
      ;; Normal objects with Rotation property
      ((vlax-property-available-p obj 'Rotation)
       (setq rotation (vlax-get obj 'Rotation))
      )
    )
    
    ;; Add to our tracking lists
    (setq original-objects (cons obj original-objects))
    (setq original-angles (cons rotation original-angles))
    
    ;; Return the rotation (not really needed but good for debugging)
    rotation
  )
  
  ;; Get original rotation index - helper for restoration
  (defun get-original-index (obj / idx count found)
    (setq idx -1)
    (setq count 0)
    (setq found nil)
    
    ;; Search the list for matching object
    (foreach item original-objects
      (if (and (not found) (equal item obj))
        (progn
          (setq idx count)
          (setq found T)
        )
      )
      (setq count (1+ count))
    )
    
    idx
  )
  
  ;; High-performance rotation function - avoids expensive operations
  (defun set-rotation (obj angle / mleader-text-changed entName)
    (setq mleader-text-changed nil)
    
    (cond
      ;; MLeader case - try property access only (faster)
      ((is-multileader obj)
       ;; Try the official method first - most reliable and fastest
       (if (vlax-property-available-p obj 'TextRotation)
         (progn
           (vl-catch-all-apply 'vlax-put (list obj 'TextRotation angle))
           (setq mleader-text-changed T)
         )
       )
       
       ;; Try alternate property
       (if (and (not mleader-text-changed) (vlax-property-available-p obj 'TextAngle))
         (progn
           (vl-catch-all-apply 'vlax-put (list obj 'TextAngle angle))
           (setq mleader-text-changed T)
         )
       )
       
       ;; Skip expensive MLEADEREDIT operations for preview (only use for final apply)
       (if (and (not mleader-text-changed) (not last-valid-preview))
         (progn
           ;; This is expensive but only used for final application, not preview
           (setq entName (vlax-vla-object->ename obj))
           (command "_.MLEADEREDIT" entName "c" "")
           (setq textEnt (entlast))
           (if textEnt
             (progn
               (setq textData (entget textEnt))
               (if (= (cdr (assoc 0 textData)) "MTEXT")
                 (progn
                   (setq textData (subst (cons 50 angle) (assoc 50 textData) textData))
                   (entmod textData)
                 )
               )
             )
           )
         )
       )
      )
      
      ;; Normal objects - direct property access (fastest)
      ((vlax-property-available-p obj 'Rotation)
       (vlax-put obj 'Rotation angle)
      )
    )
  )
  
  ;; GUARANTEED original rotation restoration - completely reliable method
  (defun force-absolute-restore (/ i obj rotation)
    ;; Only restore if we haven't already
    (if (not restored)
      (progn
        (princ "\nForce restoring original rotations...")
        
        ;; Directly iterate through all stored objects and restore rotations
        (setq i 0)
        (repeat (length original-objects)
          (setq obj (nth i original-objects))
          (setq rotation (nth i original-angles))
          
          ;; Apply restoration based on object type
          (cond
            ;; MLeader case - restore both properties
            ((is-multileader obj)
             (if (vlax-property-available-p obj 'TextRotation)
               (vlax-put obj 'TextRotation rotation)
             )
             (if (vlax-property-available-p obj 'TextAngle)
               (vlax-put obj 'TextAngle rotation)
             )
            )
            
            ;; Normal objects - restore Rotation property
            ((vlax-property-available-p obj 'Rotation)
             (vlax-put obj 'Rotation rotation)
            )
          )
          
          (setq i (1+ i))
        )
        
        ;; Ensure display is updated
        (command "_REGEN")
        
        ;; Mark as restored to prevent multiple calls
        (setq restored T)
        
        (princ "\nObjects successfully restored to original state.")
      )
    )
  )
  
  ;; Enhanced curve detection - multiple methods with support for all line types
  (defun get-curve-at-point (pt / ss p1 p2 ss2)
    ;; Create selection box around the point
    (setq p1 (list (- (car pt) selection-radius) (- (cadr pt) selection-radius)))
    (setq p2 (list (+ (car pt) selection-radius) (+ (cadr pt) selection-radius)))
    
    ;; Create filter for all line entities
    (setq filter-list (list (cons 0 line-entities)))
    
    ;; Try multiple selection methods if needed
    
    ;; First try: crossing selection (most permissive)
    (setq win-type "C")
    (setq ss (vl-catch-all-apply 'ssget (list win-type p1 p2 filter-list)))
    
    ;; Second try: window selection
    (if (or (vl-catch-all-error-p ss) (null ss) (= (sslength ss) 0))
      (progn
        (setq win-type "W")
        (setq ss (vl-catch-all-apply 'ssget (list win-type p1 p2 filter-list)))
      )
    )
    
    ;; Third try: try again with larger window
    (if (or (vl-catch-all-error-p ss) (null ss) (= (sslength ss) 0))
      (progn
        (setq p1 (list (- (car pt) (* 2 selection-radius)) (- (cadr pt) (* 2 selection-radius))))
        (setq p2 (list (+ (car pt) (* 2 selection-radius)) (+ (cadr pt) (* 2 selection-radius))))
        (setq ss (vl-catch-all-apply 'ssget (list "C" p1 p2 filter-list)))
      )
    )
    
    ;; Fourth try: use a tiny window (for precise selections)
    (if (or (vl-catch-all-error-p ss) (null ss) (= (sslength ss) 0))
      (progn
        (setq p1 (list (- (car pt) 0.5) (- (cadr pt) 0.5)))
        (setq p2 (list (+ (car pt) 0.5) (+ (cadr pt) 0.5)))
        (setq ss (vl-catch-all-apply 'ssget (list "C" p1 p2 filter-list)))
      )
    )
    
    ;; If successful and has entities, return the first one
    (if (and (not (vl-catch-all-error-p ss))
             ss
             (> (sslength ss) 0))
      (ssname ss 0)
      nil
    )
  )
  
  ;; Optimized tangent vector calculation
  (defun calculate-tangent (curveObj param / baseTanVec tanLen)
    ;; Get base tangent - only calculate once for better performance
    (setq baseTanVec (vlax-curve-getFirstDeriv curveObj param))
    (setq baseTanVec (safe-2d-point baseTanVec))
    
    ;; Normalize for accuracy without extra samples
    (setq tanLen (sqrt (+ (* (car baseTanVec) (car baseTanVec)) 
                         (* (cadr baseTanVec) (cadr baseTanVec)))))
    
    (if (> tanLen 0.0001)
      (list (/ (car baseTanVec) tanLen) (/ (cadr baseTanVec) tanLen))
      '(1.0 0.0)  ;; Default if zero length
    )
  )
  
  ;; Calculate alignment angle with visible updates
  (defun calculate-alignment (closestPt tanVec mode objData mousePt / angle objCenter objCount 
                                       mouseVec objVec crossProduct mouseCross)
    ;; Use override angle if provided
    (if angle-override
      (setq angle angle-override)
      (progn
        ;; Calculate angle from tangent vector (already normalized)
        (setq angle (atan (cadr tanVec) (car tanVec)))
        (if (= mode "Perpendicular")
          (setq angle (+ angle (/ pi 2.0)))
        )
        
        ;; Quick centroid calculation
        (setq objCenter '(0.0 0.0))
        (setq objCount 0)
        (foreach item objData
          (setq objCenter (list (+ (car objCenter) (car (cadr item)))
                               (+ (cadr objCenter) (cadr (cadr item)))))
          (setq objCount (1+ objCount))
        )
        (if (> objCount 0)
          (setq objCenter (list (/ (car objCenter) objCount) (/ (cadr objCenter) objCount)))
        )
        
        ;; Calculate vectors efficiently
        (setq mouseVec (list (- (car mousePt) (car closestPt))
                            (- (cadr mousePt) (cadr closestPt))))
        (setq objVec (list (- (car objCenter) (car closestPt))
                          (- (cadr objCenter) (cadr closestPt))))
        
        ;; Calculate cross products
        (setq crossProduct (- (* (car tanVec) (cadr objVec))
                             (* (cadr tanVec) (car objVec))))
        (setq mouseCross (- (* (car tanVec) (cadr mouseVec))
                           (* (cadr tanVec) (car mouseVec))))
        
        ;; Determine if we need to flip
        (if (< (* crossProduct mouseCross) 0)
          (setq angle (+ angle pi))
        )
      )
    )
    
    ;; Apply rotation efficiently
    (foreach item objData
      (set-rotation (car item) angle)
    )
    
    ;; Mark that we have a valid preview active
    (setq last-valid-preview T)
    
    ;; IMPORTANT: Use command REDRAW for more visible updates
    (command "_REDRAW")
    
    ;; Return the angle
    angle
  )
  
  ;; Apply view alignment with visible updates
  (defun apply-view-alignment (objData / viewAngle)
    (setq viewAngle (- 0 (getvar "viewtwist")))
    (foreach item objData
      (set-rotation (car item) viewAngle)
    )
    (command "_REDRAW")
  )
  
  ;; Handle ESC or cancel - ABSOLUTE restoration
  (defun cancel-command ()
    (princ "\nCancelling command and restoring original rotations...")
    
    ;; Use the guaranteed restoration method
    (force-absolute-restore)
    
    ;; Set done flag to exit loop
    (setq done T)
  )
  
  ;; Preview alignment - reliable approach with debug messages
  (defun preview-alignment (mousePt mode objData / ent2 curveObj closestPt param tanVec success)
    (setq success nil)
    
    (if (equal mode "View")
      ;; View mode preview
      (progn
        (apply-view-alignment objData)
        (setq success T)
      )
      
      ;; Curve alignment preview
      (progn
        ;; Try to get a curve at the current point
        (setq ent2 (get-curve-at-point mousePt))
        
        ;; If we found a curve entity
        (if ent2
          (progn
            ;; Try to convert to VLA object
            (setq curveObj (vlax-ename->vla-object ent2))
            
            (if curveObj
              (progn
                ;; Try to get closest point
                (setq closestPt (vl-catch-all-apply 'vlax-curve-getClosestPointTo (list curveObj mousePt)))
                
                (if (not (vl-catch-all-error-p closestPt))
                  (progn
                    (setq closestPt (safe-2d-point closestPt))
                    
                    (if closestPt
                      (progn
                        ;; Get curve parameter and tangent
                        (setq param (vlax-curve-getParamAtPoint curveObj closestPt))
                        (setq tanVec (calculate-tangent curveObj param))
                        
                        (if tanVec
                          (progn
                            ;; Calculate and apply alignment
                            (calculate-alignment closestPt tanVec mode objData mousePt)
                            (setq success T)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    
    ;; If we weren't successful and don't have a previous preview, restore original
    (if (and (not success) (not last-valid-preview))
      (force-absolute-restore)
    )
    
    ;; Always show options after preview
    (display-status mode)
  )
  
  ;; Status display - optimized for less overhead
  (defun display-status (mode)
    (princ "\r                                                                                ")
    (princ "\r[")
    ;; Highlight current mode
    (if (= mode "Tangent")
      (princ "TANGENT")
      (princ "T=Tangent")
    )
    (princ " | ")
    (if (= mode "Perpendicular") 
      (princ "PERPENDICULAR")
      (princ "P=Perpendicular")
    )
    (princ " | ")
    (if (= mode "View")
      (princ "VIEW")
      (princ "V=View")
    )
    (princ "] Hover for preview, click to apply, ESC to cancel [E=Exact angle]")
  )
  
  ;; Display detailed options
  (defun display-options (mode)
    (princ "\n")
    (princ "\nCurrent mode: ")
    (princ (strcase mode))
    (princ "\nOptions:")
    (princ (if (= mode "Tangent") "\n[T]angent (CURRENT)" "\n[T]angent"))
    (princ (if (= mode "Perpendicular") "\n[P]erpendicular (CURRENT)" "\n[P]erpendicular"))
    (princ (if (= mode "View") "\n[V]iew (CURRENT) - Apply and exit" "\n[V]iew - Apply and exit"))
    (princ "\n[E] Enter exact angle")
    (princ "\n[R] Reset rotation preview")
    (princ "\n[+/-] Adjust line detection sensitivity")
    (princ "\n[Click] on a line to align objects")
    (princ "\n[ESC] to cancel and restore original orientation")
    (display-status mode)
  )
  
  ;; Get a precise angle input from user
  (defun get-exact-angle (/ angle-str angle-val)
    (setq angle-str (getstring "\nEnter exact angle in degrees: "))
    (if (and angle-str (/= angle-str ""))
      (progn
        (setq angle-val (distof angle-str))
        (if (numberp angle-val)
          (* angle-val (/ pi 180.0))  ;; Convert to radians
          nil
        )
      )
      nil
    )
  )
  
  ;;; MAIN PROGRAM STARTS HERE
  
  ;; Clear command line and display banner
  (princ "\n")

 
  
  ;; Select objects
  (princ "\nSelect objects to rotate: ")
   (setq preSel (ssget "I"))
  (if (and preSel (> (sslength preSel) 0))
      (setq ss preSel)
    (progn
      (princ "\nSelect objects to rotate: ")
      (setq ss (ssget))
    )
  )

  (if (not ss)
    (progn (princ "\nNo objects selected.") (princ))
	
    (progn
      ;; Process selection - optimized for better performance
      (setq objData nil)
      (setq objCount 0)
      (setq mleaderCount 0)
      (setq i 0)
      
      ;; Clear storage variables
      (setq original-objects nil)
      (setq original-angles nil)
      
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if ent
          (progn
            (setq obj (vlax-ename->vla-object ent))
            (if obj
              (progn
                ;; Check if it's a multileader
                (if (is-multileader obj)
                  (setq mleaderCount (1+ mleaderCount))
                )
                
                ;; Get rotation point
                (setq rotPt (get-any-point obj))
                (if rotPt
                  (progn
                    (setq rotPt (safe-2d-point rotPt))
                    (if rotPt
                      (progn
                        ;; Get and store original rotation BEFORE any processing
                        (setq curAng (save-original-rotation obj))
                        
                        ;; Add to objData for processing
                        (setq objData (cons (list obj rotPt curAng) objData))
                        (setq objCount (1+ objCount))
                      )
                    )
                  )
                )
              )
            )
          )
        )
        (setq i (1+ i))
      )
	  
     ;; Start UNDO group
	(command-s "_.UNDO" "_GROUP")
	
      ;; Check for valid objects
      (if (not objData)
        (princ "\nNo valid objects found in selection.")
        
        ;; Continue with alignment interface
        (progn
          ;; Display object count summary
          (princ (strcat "\nFound " (itoa objCount) " valid objects to rotate"))
          (if (> mleaderCount 0)
            (princ (strcat " (including " (itoa mleaderCount) " multileaders)"))
          )
          (princ ".")
          
          ;; Initialize flags
          (setq restored nil)  ;; Reset restoration flag
          (setq mode "Tangent")
          (display-options mode)
          
          ;; Main interaction loop
          (setq done nil)
          (setq current-angle 0.0)
          (while (not done)
            (setq gr (grread T 15 0))
            
            (cond
              ;; Mouse movement
              ((= (car gr) 5)
               (setq mousePt (safe-2d-point (cadr gr)))
               (if mousePt 
                 (preview-alignment mousePt mode objData)
                 (display-status mode)
               )
              )
              
              ;; Left click
              ((= (car gr) 3)
               (setq clickPt (safe-2d-point (cadr gr)))
               (if clickPt
                 (progn
                   (if (equal mode "View")
                     (progn
                       (apply-view-alignment objData)
                       (setq done T)
                       (princ "\nAlignment applied (View mode).")
                     )
                     (progn
                       (setq ent2 (get-curve-at-point clickPt))
                       (if ent2
                         (progn
                           (setq curveObj (vlax-ename->vla-object ent2))
                           (if curveObj
                             (progn
                               (setq closestPt (vlax-curve-getClosestPointTo curveObj clickPt))
                               (setq param (vlax-curve-getParamAtPoint curveObj closestPt))
                               
                               ;; Use optimized tangent calculation
                               (setq tanVec (calculate-tangent curveObj param))
                               
                               (setq current-angle (calculate-alignment closestPt tanVec mode objData clickPt))
                               (setq done T)
                               (princ "\nAlignment applied.")
                             )
                           )
                         )
                         (progn
                           ;; Only apply if there's a valid preview
                           (if last-valid-preview
                             (progn
                               (setq done T)
                               (princ "\nCurrent preview alignment applied.")
                             )
                             (princ "\nNo valid line selected. Try adjusting detection radius with + or -")
                           )
                           (display-status mode)
                         )
                       )
                     )
                   )
                 )
               )
              )
              
              ;; Right click or ESC
              ((or (= (car gr) 25) (and (= (car gr) 2) (= (cadr gr) 27)))
               ;; Call dedicated cancel function - THIS IS THE KEY PART
               (cancel-command)
              )
              
              ;; Keyboard input
              ((= (car gr) 2)
               (setq keyCode (cadr gr))
               (cond
                 ;; Tangent mode (T key)
                 ((or (= keyCode 84) (= keyCode 116)) 
                  (setq mode "Tangent")
                  (setq angle-override nil) ;; Clear any angle override
                  ;; Don't restore - keep the preview
                  (display-status mode)
                 )
                 
                 ;; Perpendicular mode (P key)
                 ((or (= keyCode 80) (= keyCode 112))
                  (setq mode "Perpendicular")
                  (setq angle-override nil) ;; Clear any angle override
                  ;; Don't restore - keep the preview
                  (display-status mode)
                 )
                 
                 ;; View mode (V key) - IMMEDIATE APPLY AND EXIT
                 ((or (= keyCode 86) (= keyCode 118))
                  (setq mode "View")
                  (apply-view-alignment objData)
                  (setq done T)
                  (princ "\nAlignment applied (View mode).")
                 )
                 
                 ;; Exact angle input (E key)
                 ((or (= keyCode 69) (= keyCode 101))
                  (setq exact-angle (get-exact-angle))
                  (if exact-angle
                    (progn
                      (setq angle-override exact-angle)
                      (calculate-alignment '(0 0) '(1 0) mode objData '(0 0))
                      (princ (strcat "\nApplied exact angle: " 
                                     (angtos angle-override 0 2) 
                                     " degrees"))
                      (display-status mode)
                    )
                  )
                 )
                 
                 ;; Reset rotation preview (R key)
                 ((or (= keyCode 82) (= keyCode 114))
                  (force-absolute-restore)
                  (setq angle-override nil)
                  (setq last-valid-preview nil)
                  (princ "\nRotation preview reset.")
                  (display-status mode)
                 )
                 
                 ;; Increase selection radius (+ key)
                 ((= keyCode 43) ;; + key
                  (setq selection-radius (+ selection-radius 2))
                  (princ (strcat "\rLine detection radius increased to " (rtos selection-radius 2 0)))
                  (display-status mode)
                 )
                 
                 ;; Decrease selection radius (- key)
                 ((= keyCode 45) ;; - key
                  (if (> selection-radius 2)
                    (setq selection-radius (- selection-radius 2))
                  )
                  (princ (strcat "\rLine detection radius decreased to " (rtos selection-radius 2 0)))
                  (display-status mode)
                 )
                 
                 ;; Enter key
                 ((= keyCode 13)
                  (if (equal mode "View")
                    (progn
                      (apply-view-alignment objData)
                      (setq done T)
                      (princ "\nAlignment applied (View mode).")
                    )
                    ;; If we have a preview showing, apply it on Enter
                    (if last-valid-preview
                      (progn
                        (setq done T)
                        (princ "\nAlignment applied.")
                      )
                    )
                  )
                 )
               )
              )
            )
          )
          
          (princ (strcat "\nObjects aligned " (strcase mode) "ly."))
        )
      )
    )
  )
  
  ;; End UNDO group cleanly
(command-s "_.UNDO" "_END")
  
  (princ)
)

(princ "\nXRotate loaded. Type XRO to run.")
(princ)