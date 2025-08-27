(defun c:dmc (/ current_mode continue_flag)
  (setq current_mode "Whole" continue_flag T)
  (princ "\n--- DMC: Direct Selection Offset Tool ---")
  (princ "\nType 'S' for Segment mode, 'W' for Whole mode")
  
  (while continue_flag
    (setq result (dmc-single-operation current_mode))
    (setq current_mode (car result))
    (setq continue_flag (cadr result))
  )
  (princ "\nDMC session ended.")
  (princ)
)

(defun dmc-single-operation (mode / *error* old_cmdecho ent_sel ent_name vla_obj
                              pt1 pt2 pt_thru p_on_crv1 p_on_crv2 param1 param2
                              p_start p_end param_start param_end ent_copy ent_end_piece temp_segment
                              offset_dist offset_obj offset_vla_obj offset_start_pt offset_end_pt 
                              original_start_pt original_end_pt connector1_obj connector2_obj 
                              arrow1_obj arrow2_obj current_elevation segment_length
                              totalInches roundedInches feet inches text_content
                              mtextObj arrow_length arrow_wingspan start_tangent end_tangent
                              temp_entities_list user_input dist1 dist2
                              break_pt1 break_pt2 segment_candidates
                              facilities_layer user_osnap_setting preserve_osnap
                              key_code click_point choice segment_start_dist segment_end_dist)

  (vl-load-com)
  (setq facilities_layer "ATT-FACILITIES-PROPOSED"
        user_osnap_setting (getvar "osmode")
        preserve_osnap T
        arrow_length 19.0
        arrow_wingspan 6.25
        current_elevation 0.0)

  ;; Helper functions
  (defun calc-text-width (txt)
    (* (strlen txt) (cond ((vl-string-search "\"" txt) 14.0) ((vl-string-search "'" txt) 12.0) (T 13.0))))

  (defun add-text-mask (ent)
    (if (and ent (entget ent))
      (entmod (append (entget ent) '((90 . 3) (63 . 256) (45 . 1.25) (441 . 0))))))

  (defun ensure-layer ()
    (if (not (tblsearch "LAYER" facilities_layer))
      (entmakex (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") 
                     '(100 . "AcDbLayerTableRecord") (cons 2 facilities_layer) 
                     '(70 . 0) '(62 . 1) '(6 . "CONTINUOUS") '(370 . -3)))))

  (defun norm-vec (v)
    (if (and v (>= (length v) 3))
      (progn
        (setq m (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)) (* (caddr v) (caddr v)))))
        (if (> m 1e-10) (mapcar '/ v (list m m m)) '(0.0 0.0 0.0)))
      '(0.0 0.0 0.0)))

  (defun rot-2d (v a)
    (if (>= (length v) 2)
      (progn
        (setq x (car v) y (cadr v) c (cos a) s (sin a))
        (list (- (* x c) (* y s)) (+ (* x s) (* y c)) 0.0))
      '(0.0 0.0 0.0)))

  ;; Calculate angle from vector
  (defun vec-angle (vec)
    (if (and vec (>= (length vec) 2))
      (atan (cadr vec) (car vec))
      0.0))

  ;; Find parameter at specific distance using iterative refinement
  (defun find-param-at-distance (curve target-distance / 
                                  start-param end-param current-param current-distance
                                  low-param high-param tolerance max-iterations iteration)
    
    (setq start-param (vlax-curve-getStartParam curve)
          end-param (vlax-curve-getEndParam curve)
          tolerance 0.5
          max-iterations 30
          iteration 0
          low-param start-param
          high-param end-param
          current-param (/ (+ start-param end-param) 2.0))
    
    ;; Initialize current-distance before the loop
    (setq current-distance (vlax-curve-getDistAtParam curve current-param))
    
    ;; Use iterative refinement to find parameter at target distance
    (while (and (< iteration max-iterations)
                (> (abs (- current-distance target-distance)) tolerance))
      
      (if (< current-distance target-distance)
        (setq low-param current-param)
        (setq high-param current-param))
      
      (setq current-param (/ (+ low-param high-param) 2.0))
      (setq current-distance (vlax-curve-getDistAtParam curve current-param))
      (setq iteration (1+ iteration)))
    
    current-param)

  ;; Get true middle parameter based on arc length
  (defun get-true-middle-param (curve / total-length half-length)
    (setq total-length (vlax-curve-getDistAtParam curve (vlax-curve-getEndParam curve)))
    (setq half-length (/ total-length 2.0))
    (find-param-at-distance curve half-length))

  (defun *error* (msg)
    (if old_cmdecho (setvar "cmdecho" old_cmdecho))
    (if preserve_osnap (setvar "osmode" user_osnap_setting))
    (if temp_entities_list (foreach ent temp_entities_list (if (entget ent) (entdel ent))))
    (if (and temp_segment (entget temp_segment)) (entdel temp_segment))
    (if (and ent_end_piece (entget ent_end_piece)) (entdel ent_end_piece))
    (if (and ent_copy (entget ent_copy)) (entdel ent_copy))
    (if (not (member msg '("Function cancelled" "quit / exit abort"))) 
      (princ (strcat "\nError: " msg)))
    (list mode nil))

  ;; Create arrow
  (defun make-arrow (tip dir len wing layer clr / nd pv bp hw w1 w2)
    (if (and tip dir (> len 0) (> wing 0))
      (progn
        (setq nd (norm-vec dir))
        (setq pv (list (- (cadr nd)) (car nd) 0.0))
        (setq bp (mapcar '+ tip (mapcar '* (list len len len) nd)))
        (setq hw (/ wing 2.0))
        (setq w1 (mapcar '+ bp (mapcar '* (list hw hw hw) pv)))
        (setq w2 (mapcar '- bp (mapcar '* (list hw hw hw) pv)))
        (entmakex (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace")
                       (cons 8 layer) (cons 62 clr) '(370 . -3)
                       (cons 10 tip) (cons 11 w1) (cons 12 w2) (cons 13 w2) 
                       '(39 . 0.0) '(210 0.0 0.0 1.0))))))

  ;; Create connectors
  (defun make-connectors (ps pe ops ope ovla / start_param end_param unit_tang_start unit_tang_end
                          perp1_start perp2_start perp1_end perp2_end orig_to_off_start orig_to_off_end
                          perp_dir_start perp_dir_end conn_start1 conn_end1 conn_start2 conn_end2 c1 c2)
    (setq start_param (vlax-curve-getStartParam ovla))
    (setq end_param (vlax-curve-getEndParam ovla))
    (setq unit_tang_start (norm-vec (mapcar '- (vlax-curve-getPointAtParam ovla (min (+ start_param 0.0001) end_param)) ops)))
    (setq unit_tang_end (norm-vec (mapcar '- ope (vlax-curve-getPointAtParam ovla (max (- end_param 0.0001) start_param)))))
    (setq perp1_start (rot-2d unit_tang_start 1.5708))
    (setq perp2_start (rot-2d unit_tang_start -1.5708))
    (setq perp1_end (rot-2d unit_tang_end 1.5708))
    (setq perp2_end (rot-2d unit_tang_end -1.5708))
    (setq orig_to_off_start (norm-vec (mapcar '- ops ps)))
    (setq orig_to_off_end (norm-vec (mapcar '- ope pe)))
    (setq perp_dir_start (if (> (apply '+ (mapcar '* perp1_start orig_to_off_start)) (apply '+ (mapcar '* perp2_start orig_to_off_start))) perp1_start perp2_start))
    (setq perp_dir_end (if (> (apply '+ (mapcar '* perp1_end orig_to_off_end)) (apply '+ (mapcar '* perp2_end orig_to_off_end))) perp1_end perp2_end))
    (setq conn_start1 (mapcar '+ ps (mapcar '* '(24.0 24.0 24.0) perp_dir_start)))
    (setq conn_end1 (mapcar '+ ops (mapcar '* '(12.0 12.0 12.0) perp_dir_start)))
    (setq conn_start2 (mapcar '+ pe (mapcar '* '(24.0 24.0 24.0) perp_dir_end)))
    (setq conn_end2 (mapcar '+ ope (mapcar '* '(12.0 12.0 12.0) perp_dir_end)))
    (command "_.pline" "_non" conn_start1 "_non" conn_end1 "")
    (setq c1 (entlast))
    (if (entget c1) (progn (vla-put-layer (vlax-ename->vla-object c1) facilities_layer)
                          (vla-put-color (vlax-ename->vla-object c1) 1)
                          (vla-put-lineweight (vlax-ename->vla-object c1) acLnWtByLwDefault)))
    (command "_.pline" "_non" conn_start2 "_non" conn_end2 "")
    (setq c2 (entlast))
    (if (entget c2) (progn (vla-put-layer (vlax-ename->vla-object c2) facilities_layer)
                          (vla-put-color (vlax-ename->vla-object c2) 1)
                          (vla-put-lineweight (vlax-ename->vla-object c2) acLnWtByLwDefault)))
    (list c1 c2))

  ;; FIXED: Calculate text content using direct distance calculation for segments
  (defun calc-text (base-poly offset-poly mode-type segment-start-distance segment-end-distance / total rounded ft in)
    (setq total (cond
                  ;; Whole mode - use original polyline length
                  ((equal mode-type "Whole")
                   (vlax-curve-getDistAtParam base-poly (vlax-curve-getEndParam base-poly)))
                  ;; Segment mode - use distance difference from original polyline
                  ((and (equal mode-type "Segment") segment-start-distance segment-end-distance)
                   (abs (- segment-end-distance segment-start-distance)))
                  ;; Fallback to offset polyline length
                  (T (vlax-curve-getDistAtParam offset-poly (vlax-curve-getEndParam offset-poly)))))
    
    (princ (strcat "\n[Debug] Text calculation - Mode: " mode-type))
    (if (equal mode-type "Segment")
      (progn
        (princ (strcat "\n[Debug] Segment start distance: " (rtos segment-start-distance 2 2)))
        (princ (strcat "\n[Debug] Segment end distance: " (rtos segment-end-distance 2 2)))
        (princ (strcat "\n[Debug] Calculated segment length: " (rtos total 2 2)))
      )
      (princ (strcat "\n[Debug] Whole polyline length: " (rtos total 2 2)))
    )
    
    (setq rounded (* 6 (fix (+ (/ total 6.0) 0.5))))
    (setq ft (/ rounded 12))
    (setq in (rem rounded 12))
    (if (= in 0) (strcat (itoa ft) "'") (strcat (itoa ft) "'-" (itoa in) "\"")))

  ;; Place text at true middle of curve length
  (defun place-middle-text (poly base-poly mode-type segment-start-distance segment-end-distance / mid_param mid_pt tang angle txt width te total_length mid_distance)
    (setq total_length (vlax-curve-getDistAtParam poly (vlax-curve-getEndParam poly)))
    (princ (strcat "\n[Debug] Total length: " (rtos total_length 2 2)))
    
    (setq mid_param (get-true-middle-param poly))
    (princ (strcat "\n[Debug] Middle parameter: " (rtos mid_param 2 4)))
    
    (setq mid_pt (vlax-curve-getPointAtParam poly mid_param))
    
    ;; Verify the distance at middle parameter
    (setq mid_distance (vlax-curve-getDistAtParam poly mid_param))
    (princ (strcat "\n[Debug] Distance at middle param: " (rtos mid_distance 2 2)))
    (princ (strcat "\n[Debug] Should be half of: " (rtos (/ total_length 2.0) 2 2)))
    
    (setq tang (vlax-curve-getFirstDeriv poly mid_param))
    (setq angle (vec-angle tang))
    (setq txt (calc-text base-poly poly mode-type segment-start-distance segment-end-distance))
    (setq width (calc-text-width txt))
    (if (and (> angle 1.5708) (< angle 4.7124)) (setq angle (+ angle pi)))
    (setq te (entmakex (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                            (cons 8 facilities_layer) (cons 10 mid_pt) '(40 . 23.0)
                            (cons 41 width) (cons 1 txt) '(7 . "Arial")
                            (cons 50 angle) '(71 . 5) '(72 . 1) '(73 . 1) '(62 . 1))))
    (add-text-mask te)
    (princ (strcat " Text: " txt " [True Middle]")))

  ;; Interactive text placement with M key support during interaction
  (defun place-text-interactive (poly base-poly mode-type segment-start-distance segment-end-distance / txt continue result preview last_mouse
                                  input mouse closest param tang angle width kval te)
    (setq txt (calc-text base-poly poly mode-type segment-start-distance segment-end-distance))
    (setq continue T result nil preview nil last_mouse nil)
    (princ (strcat "\nMove cursor along line to preview text: " txt))
    (princ "\n[Type 'M' for true middle, Click to place, ESC to cancel]")
    (while continue
      (setq input (grread T 40 0))
      (cond
        ;; Mouse movement
        ((and (listp input) (= (car input) 5))
         (setq mouse (cadr input))
         (if (or (not last_mouse) (> (distance mouse last_mouse) 8.0))
           (progn
             (if (and preview (entget preview)) (entdel preview))
             (setq closest (vlax-curve-getClosestPointTo poly mouse))
             (setq param (vlax-curve-getParamAtPoint poly closest))
             (setq tang (vlax-curve-getFirstDeriv poly param))
             (setq angle (vec-angle tang))
             (setq width (calc-text-width txt))
             (if (and (> angle 1.5708) (< angle 4.7124)) (setq angle (+ angle pi)))
             (setq preview (entmakex (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                                          (cons 8 facilities_layer) (cons 10 closest) '(40 . 23.0)
                                          (cons 41 width) (cons 1 txt) '(7 . "Arial")
                                          (cons 50 angle) '(71 . 5) '(72 . 1) '(73 . 1) '(62 . 3))))
             (setq last_mouse mouse))))
        ;; Mouse click
        ((and (listp input) (= (car input) 3))
         (setq result (cadr input) continue nil)
         (princ "\n[Text placement confirmed]"))
        ;; Keyboard
        ((and (listp input) (= (car input) 2))
         (setq kval (cadr input))
         (cond
           ;; M or m key - Instant TRUE middle text placement
           ((or (= kval 77) (= kval 109))
            (if (and preview (entget preview)) (entdel preview))
            (setq result "MIDDLE" continue nil)
            (princ "\n[M pressed - True Middle text selected]"))
           ;; ESC key
           ((= kval 27) (setq continue nil) (princ "\n[Cancelled]"))
           ;; ENTER/SPACE
           ((or (= kval 13) (= kval 32))
            (if last_mouse (progn (setq result last_mouse continue nil)
                                 (princ "\n[Position confirmed]"))
              (princ "\n[No position]")))))))
    
    ;; Clean up preview
    (if (and preview (entget preview)) (entdel preview))
    
    ;; Create final text
    (cond
      ;; Middle text was selected
      ((equal result "MIDDLE")
       (place-middle-text poly base-poly mode-type segment-start-distance segment-end-distance))
      ;; Interactive position was selected
      ((and result (listp result))
       (setq closest (vlax-curve-getClosestPointTo poly result))
       (setq param (vlax-curve-getParamAtPoint poly closest))
       (setq tang (vlax-curve-getFirstDeriv poly param))
       (setq angle (vec-angle tang))
       (setq width (calc-text-width txt))
       (if (and (> angle 1.5708) (< angle 4.7124)) (setq angle (+ angle pi)))
       (setq te (entmakex (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                               (cons 8 facilities_layer) (cons 10 closest) '(40 . 23.0)
                               (cons 41 width) (cons 1 txt) '(7 . "Arial")
                               (cons 50 angle) '(71 . 5) '(72 . 1) '(73 . 1) '(62 . 1))))
       (add-text-mask te)
       (princ (strcat " Text: " txt " [Custom]")))
      ;; Cancelled or no selection
      (T (princ "\nText placement cancelled."))))

  ;; Find best segment match
  (defun find-segment (tstart tend candidates / best mindist cand cvla cs ce d1 d2 cd)
    (setq best nil mindist 999999.0)
    (foreach cand candidates
      (if (entget cand)
        (progn
          (setq cvla (vlax-ename->vla-object cand))
          (setq cs (vlax-curve-getStartPoint cvla))
          (setq ce (vlax-curve-getEndPoint cvla))
          (setq d1 (+ (distance tstart cs) (distance tend ce)))
          (setq d2 (+ (distance tstart ce) (distance tend cs)))
          (setq cd (min d1 d2))
          (if (< cd mindist) (progn (setq mindist cd) (setq best cand))))))
    best)

  ;; Main execution
  (setq temp_entities_list '() old_cmdecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (ensure-layer)

  ;; Polyline selection
  (setq ent_sel nil)
  (while (not ent_sel)
    (princ (strcat "\n[" mode " mode] Select polyline (or type 'S'/'W' to switch): "))
    (setq user_input (grread T))
    
    (cond
      ((and (listp user_input) (= (car user_input) 2))
       (setq key_code (cadr user_input))
       (cond
         ((or (= key_code 83) (= key_code 115)) (setq mode "Segment") (princ "\nSwitched to Segment mode."))
         ((or (= key_code 87) (= key_code 119)) (setq mode "Whole") (princ "\nSwitched to Whole mode."))
         ((= key_code 27) (princ "\nExiting DMC.") (setvar "cmdecho" old_cmdecho)
          (setvar "osmode" user_osnap_setting) (return (list mode nil)))
         (T (princ "\nType 'S' for Segment, 'W' for Whole, or select polyline."))))
      
      ((and (listp user_input) (= (car user_input) 3))
       (setq click_point (cadr user_input))
       (setq ent_sel (ssget click_point))
       (if ent_sel
         (progn
           (setq ent_name (ssname ent_sel 0) vla_obj (vlax-ename->vla-object ent_name))
           (if (not (member (vla-get-objectname vla_obj) '("AcDbPolyline" "AcDb2dPolyline" "AcDbSpline")))
             (progn (princ "\nInvalid object. Select a polyline.") (setq ent_sel nil))))
         (progn (princ "\nNo entity. Try again.") (setq ent_sel nil))))
      
      (T (princ "\nSelect polyline or type 'S'/'W'."))))

  ;; Execute based on mode
  (cond
    ;; WHOLE MODE
    ((= mode "Whole")
     (setq original_start_pt (vlax-curve-getStartPoint vla_obj)
           original_end_pt (vlax-curve-getEndPoint vla_obj))

     ;; Simple offset
     (setq pt_thru (getpoint "\nSpecify offset point: "))
     (if (not pt_thru) 
       (progn (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) 
              (return (list mode nil))))

     (setq offset_dist (distance (vlax-curve-getClosestPointTo vla_obj pt_thru) pt_thru))
     (if (< offset_dist 0.001)
       (progn (princ "\nOffset too small.") (setvar "cmdecho" old_cmdecho)
              (setvar "osmode" user_osnap_setting) (return (list mode T))))

     (command "_.offset" offset_dist ent_name pt_thru "")
     (setq offset_obj (entlast))
     
     (if (not (entget offset_obj))
       (progn (princ "\nOffset failed.") (setvar "cmdecho" old_cmdecho)
              (setvar "osmode" user_osnap_setting) (return (list mode T))))
     
     ;; Set properties
     (setq offset_vla_obj (vlax-ename->vla-object offset_obj))
     (vla-put-layer offset_vla_obj facilities_layer)
     (vla-put-color offset_vla_obj 1)
     (vla-put-lineweight offset_vla_obj acLnWtByLwDefault)
     (setq offset_start_pt (vlax-curve-getStartPoint offset_vla_obj)
           offset_end_pt (vlax-curve-getEndPoint offset_vla_obj))

     ;; Create connectors and arrows
     (make-connectors original_start_pt original_end_pt offset_start_pt offset_end_pt offset_vla_obj)
     
     (setq start_tangent (norm-vec (vlax-curve-getFirstDeriv offset_vla_obj (vlax-curve-getStartParam offset_vla_obj)))
           end_tangent (norm-vec (mapcar '- '(0 0 0) (vlax-curve-getFirstDeriv offset_vla_obj (vlax-curve-getEndParam offset_vla_obj)))))
     
     (make-arrow offset_start_pt start_tangent arrow_length arrow_wingspan facilities_layer 1)
     (make-arrow offset_end_pt end_tangent arrow_length arrow_wingspan facilities_layer 1)

     ;; Pass nil for segment distances in Whole mode
     (place-text-interactive offset_vla_obj vla_obj "Whole" nil nil))

    ;; SEGMENT MODE
    ((= mode "Segment")
     (setq pt1 (getpoint "\nFirst point on segment: ")
           pt2 (getpoint "\nSecond point on segment: "))
     (if (not (and pt1 pt2))
       (progn (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) 
              (return (list mode nil))))
     
     ;; FIXED: Calculate segment distances directly from original polyline
     (setq p_on_crv1 (vlax-curve-getClosestPointTo vla_obj pt1)
           p_on_crv2 (vlax-curve-getClosestPointTo vla_obj pt2)
           param1 (vlax-curve-getParamAtPoint vla_obj p_on_crv1)
           param2 (vlax-curve-getParamAtPoint vla_obj p_on_crv2)
           dist1 (vlax-curve-getDistAtParam vla_obj param1)
           dist2 (vlax-curve-getDistAtParam vla_obj param2))

     ;; Store segment start and end distances for text calculation
     (if (< dist1 dist2)
       (setq segment_start_dist dist1 segment_end_dist dist2
             p_start p_on_crv1 p_end p_on_crv2)
       (setq segment_start_dist dist2 segment_end_dist dist1
             p_start p_on_crv2 p_end p_on_crv1))

     (princ (strcat "\n[Debug] Segment distances - Start: " (rtos segment_start_dist 2 2) 
                    " End: " (rtos segment_end_dist 2 2) 
                    " Length: " (rtos (- segment_end_dist segment_start_dist) 2 2)))

     (if (< (abs (- dist1 dist2)) 0.001)
       (progn (princ "\nSegment too short.") (setvar "cmdecho" old_cmdecho)
              (setvar "osmode" user_osnap_setting) (return (list mode T))))

     ;; Break polyline (for offset purposes)
     (command "_.copy" ent_name "" "_non" "0,0,0" "_non" "0,0,0")
     (setq ent_copy (entlast) temp_entities_list (list ent_copy))

     (command "_.break" ent_copy "_non" p_end "_non" p_end)
     (setq ent_end_piece (entlast))
     (if (not (equal ent_end_piece ent_copy)) (setq temp_entities_list (cons ent_end_piece temp_entities_list)))

     (command "_.break" ent_copy "_non" p_start "_non" p_start)
     (setq temp_segment (entlast))
     (if (not (equal temp_segment ent_copy)) (setq temp_entities_list (cons temp_segment temp_entities_list)))

     (setq temp_segment (find-segment p_start p_end (append (list ent_copy) 
                                                           (if ent_end_piece (list ent_end_piece) '())
                                                           (if temp_segment (list temp_segment) '()))))

     (if (not (entget temp_segment))
       (progn (princ "\nSegment identification failed.")
              (foreach ent temp_entities_list (if (entget ent) (entdel ent)))
              (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) (return (list mode T))))

     ;; Offset segment
     (setq temp_segment_vla (vlax-ename->vla-object temp_segment)
           pt_thru (getpoint "\nSpecify offset point: "))
     
     (if (not pt_thru)
       (progn (foreach ent temp_entities_list (if (entget ent) (entdel ent)))
              (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) (return (list mode nil))))

     (setq offset_dist (distance (vlax-curve-getClosestPointTo temp_segment_vla pt_thru) pt_thru))
     (if (< offset_dist 0.001)
       (progn (princ "\nOffset too small.")
              (foreach ent temp_entities_list (if (entget ent) (entdel ent)))
              (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) (return (list mode T))))

     (command "_.offset" offset_dist temp_segment "_non" pt_thru "")
     (setq offset_obj (entlast))
     
     (if (not (entget offset_obj))
       (progn (princ "\nOffset failed.")
              (foreach ent temp_entities_list (if (entget ent) (entdel ent)))
              (setvar "cmdecho" old_cmdecho) (setvar "osmode" user_osnap_setting) (return (list mode T))))
     
     ;; Set properties
     (setq offset_vla_obj (vlax-ename->vla-object offset_obj))
     (vla-put-layer offset_vla_obj facilities_layer)
     (vla-put-color offset_vla_obj 1)
     (vla-put-lineweight offset_vla_obj acLnWtByLwDefault)
     (setq offset_start_pt (vlax-curve-getStartPoint offset_vla_obj)
           offset_end_pt (vlax-curve-getEndPoint offset_vla_obj))

     ;; Create connectors and arrows
     (make-connectors p_start p_end offset_start_pt offset_end_pt offset_vla_obj)
     
     (setq start_tangent (norm-vec (vlax-curve-getFirstDeriv offset_vla_obj (vlax-curve-getStartParam offset_vla_obj)))
           end_tangent (norm-vec (mapcar '- '(0 0 0) (vlax-curve-getFirstDeriv offset_vla_obj (vlax-curve-getEndParam offset_vla_obj)))))
     
     (make-arrow offset_start_pt start_tangent arrow_length arrow_wingspan facilities_layer 1)
     (make-arrow offset_end_pt end_tangent arrow_length arrow_wingspan facilities_layer 1)

     ;; FIXED: Pass the calculated segment distances for accurate length calculation
     (place-text-interactive offset_vla_obj vla_obj "Segment" segment_start_dist segment_end_dist)

     ;; Clean up
     (foreach ent temp_entities_list (if (entget ent) (entdel ent)))))

  (setvar "cmdecho" old_cmdecho)
  (setvar "osmode" user_osnap_setting)
  (list mode T))

;;; Load message
(princ "\n--- DimCurve.lsp loaded. Type DMC to run. ---")

(princ)