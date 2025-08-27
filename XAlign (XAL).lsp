(vl-load-com)

(setq *XADEBUG* nil) ; Set to T for debugging

(defun xa:dbg (s) (if *XADEBUG* (princ (strcat "\n[DEBUG] " s))) (princ))
(defun xa:msg (s) (princ s) (princ))

; Error handler
(defun xa:err (m)
  (if (= m "Function cancelled")
    (xa:msg "\n*Cancelled*")
    (xa:msg (strcat "\nError: " m))
  )
  (setq *error* oldError)
  (princ)
)

; Vector math helpers
(defun xa:v- (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b)) 0.0))
(defun xa:vrot (v ang) 
  (list (- (* (car v) (cos ang)) (* (cadr v) (sin ang)))
        (+ (* (car v) (sin ang)) (* (cadr v) (cos ang)))
        0.0)
)
(defun xa:dist2 (a b / d) 
  (setq d (xa:v- a b)) 
  (+ (* (car d) (car d)) (* (cadr d) (cadr d)))
)

; UI display
(defun xa:show-button-display (mode refAngle copyMode / tangentBtn perpBtn copyBtn refBtn displayLine)
  (setq tangentBtn (if (eq mode "TANGENT") "[TANGENT]" "Tangent"))
  (setq perpBtn (if (eq mode "PERP") "[PERPENDICULAR]" "Perpendicular"))
  (setq copyBtn (if copyMode "[COPY]" "Move"))
  (setq refBtn (strcat "Ref:" (rtos (* refAngle (/ 180 pi)) 2 1) "°"))
  (setq displayLine (strcat tangentBtn "|" perpBtn "|" copyBtn "|" refBtn "|X"))
  (princ (strcat "\r" displayLine))
)

; Reference angle detection from selection
(defun xa:detect-reference-angle (ss / i ent entData entType angles commonAngle tolerance refAngle)
  (setq angles '() tolerance 0.01745 i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq entData (entget ent))
    (setq entType (cdr (assoc 0 entData)))
    (cond
      ((or (eq entType "TEXT") (eq entType "MTEXT") (eq entType "INSERT"))
       (if (assoc 50 entData)
         (setq angles (cons (cdr (assoc 50 entData)) angles))
       )
      )
      ((eq entType "LINE")
       (setq refAngle (angle (cdr (assoc 10 entData)) (cdr (assoc 11 entData))))
       (setq angles (cons refAngle angles))
      )
    )
    (setq i (1+ i))
  )
  
  (setq commonAngle 0.0)
  (if angles
    (progn
      (setq commonAngle (car angles) i 1)
      (while (< i (length angles))
        (if (> (abs (- commonAngle (nth i angles))) tolerance)
          (setq commonAngle 0.0)
        )
        (setq i (1+ i))
      )
    )
  )
  
  (if (and angles (> (length angles) 0)) commonAngle 0.0)
)

; Reference angle editing
(defun xa:cycle-reference-angle (currentRefAngle originalRefAngle editMode / newRefAngle)
  (cond
    ((= editMode 0)
     (xa:msg "\nSet reference angle:")
     (setq newRefAngle (getangle (strcat "\nReference angle <" 
                                        (rtos (* currentRefAngle (/ 180 pi)) 2 1) 
                                        ">: ")))
     (if newRefAngle
       (progn
         (xa:msg (strcat "\nReference angle: " (rtos (* newRefAngle (/ 180 pi)) 2 1) "°"))
         newRefAngle
       )
       (progn
         (xa:msg "\nReference angle unchanged")
         currentRefAngle
       )
     )
    )
    ((= editMode 1)
     (xa:msg (strcat "\nReset to: " (rtos (* originalRefAngle (/ 180 pi)) 2 1) "°"))
     originalRefAngle
    )
    (T currentRefAngle)
  )
)

; Tangent calculation with reference compensation
(defun xa:get-tangent-angle-with-reference (curve projPt cursorPt modePerp refAngle / param deriv baseAngle curveNormal approachVec cross)
  (if (or (null curve) (null (entget curve)))
    (- 0.0 refAngle)
    (progn
      (setq param (vlax-curve-getParamAtPoint curve projPt))
      (setq deriv (vlax-curve-getFirstDeriv curve param))
      
      (if (or (null deriv) (equal deriv '(0 0 0) 1e-12))
        (- 0.0 refAngle)
        (progn
          (setq baseAngle (angle '(0 0 0) (list (car deriv) (cadr deriv) 0.0)))
          (if modePerp (setq baseAngle (+ baseAngle (/ pi 2.0))))
          (setq curveNormal (list (- (sin baseAngle)) (cos baseAngle) 0.0))
          (setq approachVec (xa:v- projPt cursorPt))
          (setq cross (+ (* (car curveNormal) (car approachVec)) 
                         (* (cadr curveNormal) (cadr approachVec))))
          (if (< cross 0.0) (setq baseAngle (+ baseAngle pi)))
          (setq baseAngle (- baseAngle refAngle))
          (while (< baseAngle 0) (setq baseAngle (+ baseAngle (* 2 pi))))
          (while (>= baseAngle (* 2 pi)) (setq baseAngle (- baseAngle (* 2 pi))))
          baseAngle
        )
      )
    )
  )
)

; Optimized temp block creation
(defun xa:create-temp-block (ss basePt blkName / oldEcho copyLastEnt copySelection currentEnt)
  (setq oldEcho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (setq copyLastEnt (entlast))
  
  (command "._COPY" ss "" basePt basePt)
  
  (setq copySelection (ssadd))
  (setq currentEnt (if copyLastEnt (entnext copyLastEnt) (entnext)))
  (while currentEnt
    (ssadd currentEnt copySelection)
    (setq currentEnt (entnext currentEnt))
  )
  
  (if (> (sslength copySelection) 0)
    (command "._BLOCK" blkName basePt copySelection "")
  )
  
  (setvar "CMDECHO" oldEcho)
  (tblsearch "BLOCK" blkName)
)

; ActiveX helpers
(defun xa:get-space (doc)
  (if (= (getvar "CVPORT") 1)
    (vla-get-PaperSpace doc)
    (vla-get-ModelSpace doc)
  )
)

(defun xa:insert-block (blkName insPt / acadApp doc space)
  (setq acadApp (vlax-get-acad-object))
  (setq doc (vla-get-ActiveDocument acadApp))
  (setq space (xa:get-space doc))
  (vlax-vla-object->ename 
    (vla-InsertBlock space (vlax-3d-point insPt) blkName 1.0 1.0 1.0 0.0)
  )
)

(defun xa:explode-block (blkEnt / vlaObj)
  (if (and blkEnt (entget blkEnt))
    (progn
      (setq vlaObj (vlax-ename->vla-object blkEnt))
      (vlax-invoke vlaObj 'Explode)
      (vla-Delete vlaObj)
      T
    )
  )
)

(defun xa:purge-block (blkName / acadApp doc blks blk)
  (if (and blkName (/= blkName ""))
    (progn
      (setq acadApp (vlax-get-acad-object))
      (setq doc (vla-get-ActiveDocument acadApp))
      (setq blks (vla-get-Blocks doc))
      (setq blk (vl-catch-all-apply 'vla-Item (list blks blkName)))
      (if (not (vl-catch-all-error-p blk))
        (vl-catch-all-apply 'vla-Delete (list blk))
      )
    )
  )
)

; Perfect copy by copying preview block
(defun xa:copy-preview-block (tmpBlkRef / vlaObj vlaCopy)
  (if (and tmpBlkRef (entget tmpBlkRef))
    (progn
      (setq vlaObj (vlax-ename->vla-object tmpBlkRef))
      (setq vlaCopy (vl-catch-all-apply 'vla-Copy (list vlaObj)))
      (if (not (vl-catch-all-error-p vlaCopy))
        (progn
          (vlax-invoke vlaCopy 'Explode)
          (vla-Delete vlaCopy)
          T
        )
      )
    )
  )
)

; Curve detection
(defun xa:pickbox-tol (/ pb vs sc h)
  (setq pb (getvar "PICKBOX"))
  (setq vs (getvar "VIEWSIZE"))
  (setq sc (getvar "SCREENSIZE"))
  (if (and (listp sc) (= (length sc) 2))
    (progn
      (setq h (cadr sc))
      (if (and h (> h 0))
        (* vs (/ pb h))
        (* vs 0.01)
      )
    )
    (* vs 0.01)
  )
)

(defun xa:nearest-curve (pt size / p1 p2 ss filt best bestd i en d cp)
  (setq p1 (list (- (car pt) size) (- (cadr pt) size)))
  (setq p2 (list (+ (car pt) size) (+ (cadr pt) size)))
  (setq filt (list (cons 0 "LINE,ARC,LWPOLYLINE,POLYLINE,CIRCLE,ELLIPSE,SPLINE,RAY,XLINE,MLINE")))
  (setq ss (ssget "_C" p1 p2 filt))
  (if (and ss (> (sslength ss) 0))
    (progn
      (setq i 0 best nil bestd 1e99)
      (while (< i (sslength ss))
        (setq en (ssname ss i))
        (setq cp 
          (vl-catch-all-apply
            '(lambda () (vlax-curve-getClosestPointTo en pt))
            '()
          )
        )
        (if (not (vl-catch-all-error-p cp))
          (progn
            (setq d (xa:dist2 pt cp))
            (if (< d bestd)
              (progn (setq best en bestd d))
            )
          )
        )
        (setq i (1+ i))
      )
      best
    )
  )
)

(defun xa:get-projection (curve pt)
  (if (and curve (entget curve))
    (vl-catch-all-apply
      '(lambda () (vlax-curve-getClosestPointTo curve pt))
      '()
    )
    pt
  )
)

; MAIN COMMAND - FIXED PRESELECTION + PROPER SINGLE UNDO
(defun c:XAL (/ oldError ss preSel cnt singleBlock basePt selectionRadius tol 
                canceled accept mode copyMode tmpBlkName tmpBlkRef origIns origRot localBase
                cursorWcs curveEnt projPt newAngle newIns curDxf ev key done 
                rotatedLocal blkOrigDxf referenceAngle originalReferenceAngle 
                refEditMode copyCount originalLastEnt finished i ent)

  (setq oldError *error*)
  (setq *error* xa:err)

 

  ; Store last entity for cleanup tracking
  (setq originalLastEnt (entlast))

  ; FIXED: Proper preselection support
  (setq preSel (ssget "I"))
  (if (and preSel (> (sslength preSel) 0))
    (progn
      (xa:msg "\nUsing preselected objects.")
      (setq ss preSel)
    )
    (progn
      (xa:msg "\nSelect objects to align:")
      (setq ss (ssget))
    )
  )
  
  (if (not ss)
    (progn 
      (xa:msg "\nNothing selected.")
      (command "._UNDO" "_End")
      (setq *error* oldError) 
      (princ) 
      (exit)
    )
  )
 ; PROPER SINGLE UNDO - START WITH BEGIN
  (command "._UNDO" "_Begin")
  (setq cnt (sslength ss))
  (xa:dbg (strcat "Selected " (itoa cnt) " objects"))
  
  ; Initialize variables
  (setq referenceAngle (xa:detect-reference-angle ss))
  (setq originalReferenceAngle referenceAngle)
  (setq refEditMode 0)
  (setq copyMode nil)
  (setq copyCount 0)
  (setq finished nil)
  
  ; Check for single block
  (if (= cnt 1)
    (progn
      (setq singleBlock (ssname ss 0))
      (if (not (eq (strcase (cdr (assoc 0 (entget singleBlock)))) "INSERT"))
        (setq singleBlock nil)
      )
    )
  )

  ; Get base point
  (setq basePt (getpoint "\nSpecify base point on selection: "))
  (if (not basePt)
    (progn 
      (xa:msg "\nCancelled.") 
      (command "._UNDO" "_End")
      (command "._U")
      (command "._U")	  
      (setq *error* oldError) 
      (princ) 
      (exit)
    )
  )

  (setq selectionRadius 5)
  (setq tol (xa:pickbox-tol))
  (setq mode "TANGENT" canceled nil accept nil)

  ; Setup temporary block or use single block
  (if singleBlock
    (progn
      (xa:dbg "Single block mode")
      (setq blkOrigDxf (entget singleBlock))
      (setq tmpBlkRef singleBlock)
      (setq origIns (cdr (assoc 10 blkOrigDxf)))
      (setq origRot (if (assoc 50 blkOrigDxf) (cdr (assoc 50 blkOrigDxf)) 0.0))
      (setq localBase (xa:vrot (xa:v- basePt origIns) (- origRot)))
      (setq tmpBlkName nil)
    )
    (progn
      (xa:dbg "Multiple entity mode")
      (setq tmpBlkName (strcat "XALIGN_TMP_" (rtos (getvar "DATE") 2 6)))
      
      (if (not (xa:create-temp-block ss basePt tmpBlkName))
        (progn
          (xa:msg "\nFailed to create temporary block.")
          (command "._UNDO" "_End")
          (command "._U")
          (setq *error* oldError)
          (princ) 
          (exit)
        )
      )
      
      (setq tmpBlkRef (xa:insert-block tmpBlkName basePt))
      (if (null tmpBlkRef)
        (progn
          (xa:msg "\nFailed to insert temporary block.")
          (xa:purge-block tmpBlkName)
          (command "._UNDO" "_End")
          (command "._U")
          (setq *error* oldError)
          (princ) 
          (exit)
        )
      )
      
      (setq localBase '(0.0 0.0 0.0))
      (setq origIns basePt)
      (setq origRot 0.0)
    )
  )

  (xa:msg "\n🚀 XALIGN - Fixed!")
  (xa:msg (strcat "\nReference: " (rtos (* referenceAngle (/ 180 pi)) 2 1) "°"))
  (xa:msg "\nT=Tangent P=Perp C=Copy/Move R=RefAngle")

  (setq done nil newAngle origRot)

  ; Main interaction loop
  (while (not done)
    (setq ev (grread T 15 0))
    (cond
      ((= (car ev) 5) ; mouse move
       (if (null (entget tmpBlkRef))
         (setq canceled T done T)
         (progn
           (setq cursorWcs (trans (cadr ev) 1 0))
           (if (null cursorWcs) (setq cursorWcs basePt))
           
           (setq curveEnt (xa:nearest-curve cursorWcs (* selectionRadius tol)))
           (xa:show-button-display mode referenceAngle copyMode)
           
           (if (and curveEnt (entget curveEnt))
             (progn
               (setq projPt (xa:get-projection curveEnt cursorWcs))
               (setq newAngle 
                 (xa:get-tangent-angle-with-reference 
                   curveEnt projPt cursorWcs (eq mode "PERP") referenceAngle
                 )
               )
             )
             (progn
               (setq projPt cursorWcs)
               (setq newAngle (- 0.0 referenceAngle))
             )
           )
           
           (if (not (numberp newAngle)) (setq newAngle 0.0))
           (setq rotatedLocal (xa:vrot localBase newAngle))
           (setq newIns (xa:v- projPt rotatedLocal))
           (setq curDxf (entget tmpBlkRef))
           (if curDxf
             (progn
               (setq curDxf (subst (cons 10 newIns) (assoc 10 curDxf) curDxf))
               (if (assoc 50 curDxf)
                 (setq curDxf (subst (cons 50 newAngle) (assoc 50 curDxf) curDxf))
                 (setq curDxf (append curDxf (list (cons 50 newAngle))))
               )
               (entmod curDxf)
               (entupd tmpBlkRef)
             )
           )
         )
       )
      )
      
      ((= (car ev) 3) ; mouse click
       (if copyMode
         (progn
           (setq copyCount (1+ copyCount))
           (xa:copy-preview-block tmpBlkRef)
           (xa:msg (strcat "\nCopy " (itoa copyCount) " created!"))
           (princ "\n")
         )
         (progn
           (setq accept T done T)
           (princ "\n")
         )
       )
      )
      
      ((= (car ev) 25) ; right click
       (if (= (cadr ev) 2)
         (if copyMode
           (progn
             (setq finished T done T)
             (xa:msg (strcat "\nFinished - " (itoa copyCount) " copies!"))
             (princ "\n")
           )
           (progn
             (setq canceled T done T)
             (princ "\n")
           )
         )
       )
      )
      
      ((= (car ev) 2) ; keyboard
       (setq key (chr (cadr ev)))
       (cond
         ((wcmatch (strcase key) "X") 
          (setq canceled T done T)
          (princ "\n")
         )
         ((wcmatch (strcase key) "T") 
          (setq mode "TANGENT") 
          (princ "\n")
          (xa:msg "Mode: TANGENT")
         )
         ((wcmatch (strcase key) "P") 
          (setq mode "PERP") 
          (princ "\n")
          (xa:msg "Mode: PERPENDICULAR")
         )
         ((wcmatch (strcase key) "C") 
          (setq copyMode (not copyMode))
          (princ "\n")
          (if copyMode
            (xa:msg "📋 COPY MODE: Creates copies!")
            (xa:msg "🚚 MOVE MODE: Moves originals!")
          )
         )
         ((wcmatch (strcase key) "R") 
          (princ "\n")
          (setq referenceAngle (xa:cycle-reference-angle referenceAngle originalReferenceAngle refEditMode))
          (setq refEditMode (rem (+ refEditMode 1) 2))
         )
         ((wcmatch key "+")
          (setq selectionRadius (+ selectionRadius 2))
          (princ "\n")
          (xa:msg (strcat "Detection radius: " (itoa selectionRadius)))
         )
         ((wcmatch key "-")
          (if (> selectionRadius 2) (setq selectionRadius (- selectionRadius 2)))
          (princ "\n")
          (xa:msg (strcat "Detection radius: " (itoa selectionRadius)))
         )
         (T 
          (if (= (cadr ev) 13) 
            (if copyMode
              (progn
                (setq finished T done T)
                (xa:msg (strcat "\nFinished - " (itoa copyCount) " copies!"))
                (princ "\n")
              )
              (progn
                (setq accept T done T)
                (princ "\n")
              )
            )
          )
         )
       )
      )
      
      ((= (car ev) 1) ; function keys
       (if (= (cadr ev) 27) ; ESC
         (progn
           (setq canceled T done T)
           (princ "\n")
         )
       )
      )
    )
  )

  ; Finalize based on exit condition
  (cond
    (canceled
      (xa:dbg "CANCELED")
      ; Clean up temp elements
      (if (not singleBlock)
        (progn
          (if (and tmpBlkRef (entget tmpBlkRef)) (entdel tmpBlkRef))
          (if tmpBlkName (xa:purge-block tmpBlkName))
        )
        (if (and blkOrigDxf (entget tmpBlkRef))
          (progn
            (entmod blkOrigDxf)
            (entupd tmpBlkRef)
          )
        )
      )
      ; Delete any copies created
      (setq i (if originalLastEnt (entnext originalLastEnt) (entnext)))
      (while i
        (entdel i)
        (setq i (entnext i))
      )
      (command "._UNDO" "_End")
      (command "._U")
      (xa:msg "\nCANCELLED - All changes undone")
    )
    ((or finished accept)
      (xa:dbg (if finished "FINISHED" "ACCEPTED"))
      (if singleBlock
        (xa:msg "\n✅ Single block aligned!")
        (progn
          ; For multiple entities: delete originals and explode temp block
          (if accept
            (progn
              (setq i 0)
              (while (< i (sslength ss))
                (entdel (ssname ss i))
                (setq i (1+ i))
              )
              (xa:explode-block tmpBlkRef)
              (xa:purge-block tmpBlkName)
              (xa:msg "\n✅ MOVE complete!")
            )
            (progn
              ; Just clean up temp block for copy mode
              (if (and tmpBlkRef (entget tmpBlkRef)) (entdel tmpBlkRef))
              (if tmpBlkName (xa:purge-block tmpBlkName))
              (xa:msg (strcat "\n✅ FINISHED - " (itoa copyCount) " copies!"))
            )
          )
        )
      )
      ; END UNDO GROUP
      (command "._UNDO" "_End")
    )
  )

  (command "_REDRAW")
  (setq *error* oldError)
  (princ)
)

(princ "\nXALIGN loaded")
(princ "\nType XAL to start.")
(princ)