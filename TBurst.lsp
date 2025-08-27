(defun c:tburst (/ *error* old_cmdecho old_osmode selection_set i block_ent
                    block_attrs attr_entities exploded_entities
                    ent_data ent_type entities_before entities_after new_entities
                    view_angle)
  (vl-load-com)

  ;; --- Error Handler ---
  (defun *error* (msg)
    (if old_cmdecho (setvar "cmdecho" old_cmdecho))
    (if old_osmode (setvar "osmode" old_osmode))

    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\nError in XBURST: " msg))
    )
    (princ)
  )

  (defun allowed-block-p (block_name)
    (if (not block_name) 
      nil
      (progn

        (setq block_name_upper (strcase block_name))
        (or (= block_name_upper "TRAFFIC FLOW DYNA")
            (= block_name_upper "PAINTED ARROW S DYNA")
            (= block_name_upper "PAINTED ARROW M DYNA")
            (= block_name_upper "PAINTED ARROW L DYNA")
            (= block_name_upper "PAINTED ARROW XL DYNA"))
      )
    )
  )

  ;; --- Get effective block name (handles dynamic blocks) ---
  (defun get-block-name (block_ent)
    (if (and block_ent (entget block_ent))
      (progn
        (setq block_data (entget block_ent))
        ;; Try to get block name (DXF group code 2)
        (setq block_name (cdr (assoc 2 block_data)))
        
        ;; If it's a dynamic block, also check for the dynamic block name
        (if (vlax-property-available-p 
              (setq obj (vlax-ename->vla-object block_ent)) 
              'EffectiveName)
          (setq dynamic_name (vla-get-EffectiveName obj))
          (setq dynamic_name nil)
        )
        
        ;; Return either dynamic name or regular name
        (if (and dynamic_name (> (strlen dynamic_name) 0))
          dynamic_name
          block_name
        )
      )
      nil
    )
  )

  ;; --- Check if ARIAL text style exists ---
  (defun check-arial-style ()
    (if (tblsearch "STYLE" "ARIAL")
      (progn
        (princ "\nUsing existing ARIAL text style")
        "ARIAL"
      )
      (progn
        (princ "\nWarning: ARIAL text style not found, using Standard")
        "Standard"
      )
    )
  )

  ;; --- Safely get DXF group value ---
  (defun safe-dxf (group_code ent_data)
    (setq result (assoc group_code ent_data))
    (if result (cdr result) nil)
  )

  ;; --- Get all entities for comparison ---
  (defun get-all-entities ()
    (setq all_ents '())
    (setq ent (entnext))
    (while ent
      (setq all_ents (cons ent all_ents))
      (setq ent (entnext ent))
    )
    all_ents
  )

  ;; --- Get new entities after explode ---
  (defun get-new-entities (old_entities)
    (setq new_ents '())
    (setq ent (entnext))
    (while ent
      (if (not (member ent old_entities))
        (setq new_ents (cons ent new_ents))
      )
      (setq ent (entnext ent))
    )
    new_ents
  )

  ;; --- Get the correct text position from ATTRIB ---
  (defun get-text-position (ent_data)
    (setq insertion_pt (safe-dxf 10 ent_data))    ; Primary insertion point
    (setq alignment_pt (safe-dxf 11 ent_data))    ; Alignment point (if exists)
    (setq alignment_code (safe-dxf 72 ent_data))  ; Horizontal alignment
    (setq vertical_align (safe-dxf 74 ent_data))  ; Vertical alignment
    
    ;; If alignment point exists and alignment is not left-baseline, use alignment point
    (if (and alignment_pt 
             alignment_code 
             (or (/= alignment_code 0) vertical_align))
      alignment_pt      ; Use alignment point for positioned text
      insertion_pt      ; Use insertion point for default positioned text
    )
  )

  ;; --- Get block attributes using entity method ---
  (defun get-block-attributes-entity (block_ent)
    (setq attr_list '())
    (setq next_ent (entnext block_ent))
    
    ;; Collect all ATTRIB entities following the block
    (while (and next_ent 
                (setq ent_data (entget next_ent))
                (= (safe-dxf 0 ent_data) "ATTRIB"))
      
      ;; Get the correct position based on alignment
      (setq correct_position (get-text-position ent_data))
      
      ;; Store attribute data with all properties
      (setq attr_info (list
        (cons "ENTITY" next_ent)
        (cons "TAG" (safe-dxf 2 ent_data))
        (cons "TEXT" (safe-dxf 1 ent_data))
        (cons "POSITION" correct_position)             ; Use correct position
        (cons "HEIGHT" (safe-dxf 40 ent_data))
        (cons "ROTATION" (safe-dxf 50 ent_data))
        (cons "LAYER" (safe-dxf 8 ent_data))
        (cons "COLOR" (safe-dxf 62 ent_data))
        (cons "LINETYPE" (safe-dxf 6 ent_data))
        (cons "WIDTH" (safe-dxf 41 ent_data))          ; Width factor
        (cons "OBLIQUE" (safe-dxf 51 ent_data))        ; Oblique angle
      ))
      
      (setq attr_list (cons attr_info attr_list))
      (setq next_ent (entnext next_ent))
    )
    
    attr_list
  )

  ;; --- Get horizontal angle based on current view ---
  (defun get-view-horizontal-angle ()
    ;; Get view twist and negate it to get angle needed for horizontal text
    (setq viewAngle (- 0 (getvar "viewtwist")))
    viewAngle
  )

  ;; --- Create TEXT from attribute data with ARIAL style ---
  (defun create-text-from-attribute (attr_data text_style)
    (setq tag_name (cdr (assoc "TAG" attr_data)))
    (setq text_string (cdr (assoc "TEXT" attr_data)))
    (setq position (cdr (assoc "POSITION" attr_data)))
    (setq height (cdr (assoc "HEIGHT" attr_data)))
    (setq layer_name (cdr (assoc "LAYER" attr_data)))
    (setq color_value (cdr (assoc "COLOR" attr_data)))
    (setq linetype_name (cdr (assoc "LINETYPE" attr_data)))
    (setq width_factor (cdr (assoc "WIDTH" attr_data)))
    (setq oblique_angle (cdr (assoc "OBLIQUE" attr_data)))
    
    (if (and position height text_string)
      (progn
        ;; Create TEXT entity with ARIAL style and preserved properties
        (setq text_data (list 
          (cons 0 "TEXT")
          (cons 10 position)                    ; Use correct position
          (cons 11 position)                    ; Alignment point same as insertion for middle center
          (cons 40 height)                      ; Text height
          (cons 50 0.0)                         ; Initially set to 0, will rotate later
          (cons 1 text_string)                  ; Text string
          (cons 7 text_style)                   ; Use ARIAL text style
          (cons 72 1)                           ; Horizontal alignment: 1 = Center
          (cons 73 2)                           ; Vertical alignment: 2 = Middle
        ))
        
        ;; Add layer (use "0" if none specified)
        (setq text_data (append text_data (list (cons 8 (if layer_name layer_name "0")))))
        
        ;; Add color if specified and not BYLAYER
        (if (and color_value (/= color_value 256))
          (setq text_data (append text_data (list (cons 62 color_value))))
        )
        
        ;; Add linetype if specified
        (if linetype_name
          (setq text_data (append text_data (list (cons 6 linetype_name))))
        )
        
        ;; Add width factor if specified
        (if (and width_factor (/= width_factor 1.0))
          (setq text_data (append text_data (list (cons 41 width_factor))))
        )
        
        ;; Add oblique angle if specified
        (if (and oblique_angle (/= oblique_angle 0.0))
          (setq text_data (append text_data (list (cons 51 oblique_angle))))
        )
        
        ;; Create the text entity
        (setq new_text_ent (entmakex text_data))
        
        (princ (strcat " [" (if tag_name tag_name "ATTR") ":" text_string "]"))
        new_text_ent
      )
      (progn
        (princ " [SKIP:Invalid attr]")
        nil
      )
    )
  )

  ;; --- Rotate text to be horizontal to current view ---
  (defun rotate-text-to-view (text_ent view_angle)
    (if text_ent
      (progn
        (setq text_data (entget text_ent))
        (setq rotated_data (subst (cons 50 view_angle) (assoc 50 text_data) text_data))
        (entmod rotated_data)
      )
    )
  )

  ;; --- Process a single block ---
  (defun xburst-single-block (block_ent text_style view_angle)
    (if (and block_ent (entget block_ent))
      (progn
        ;; Get both regular name and effective name (for dynamic blocks)
        (setq block_name (get-block-name block_ent))
        
        ;; Display the block name we found
        (if (not block_name) (setq block_name "Anonymous"))
        
        ;; Only process if block is in the allowed list
        (if (allowed-block-p block_name)
          (progn
            (princ (strcat "\nProcessing: " block_name))
            
            ;; Step 1: Get attribute data BEFORE exploding
            (setq attribute_data (get-block-attributes-entity block_ent))
            (setq attr_count (length attribute_data))
            
            (if (> attr_count 0)
              (princ (strcat " [" (itoa attr_count) " attrs]"))
            )
            
            ;; Step 2: Get entities before explode
            (setq entities_before (get-all-entities))
            
            ;; Step 3: Explode the block
            (princ " [EXPLODING]")
            (command "_.explode" block_ent)
            
            ;; Step 4: Get new entities after explode
            (setq entities_after (get-all-entities))
            (setq new_entities (get-new-entities entities_before))
            
            (princ (strcat " [" (itoa (length new_entities)) " entities]"))
            
            ;; Step 5: Remove any ATTDEF entities that were created by explode
            (setq attdef_count 0)
            (foreach new_ent new_entities
              (if (and new_ent (entget new_ent))
                (progn
                  (setq ent_data (entget new_ent))
                  (setq ent_type (safe-dxf 0 ent_data))
                  
                  ;; Delete ATTDEF entities (we'll replace with our TEXT)
                  (if (= ent_type "ATTDEF")
                    (progn
                      (entdel new_ent)
                      (setq attdef_count (1+ attdef_count))
                    )
                  )
                )
              )
            )
            
            (if (> attdef_count 0)
              (princ (strcat " [Removed " (itoa attdef_count) " ATTDEF]"))
            )
            
            ;; Step 6: Create TEXT entities with ARIAL style and then rotate them
            (setq text_count 0)
            (setq text_entities '())
            (foreach attr_data attribute_data
              (setq new_text (create-text-from-attribute attr_data text_style))
              (if new_text
                (progn
                  (setq text_entities (cons new_text text_entities))
                  (setq text_count (1+ text_count))
                )
              )
            )
            
            ;; Step 7: Rotate created text to be horizontal to current view
            (foreach text_ent text_entities
              (rotate-text-to-view text_ent view_angle)
            )
            
            (if (> text_count 0)
              (princ (strcat " [Created " (itoa text_count) " " text_style " TEXT]"))
            )
            
            (princ " ✓")
            T
          )
          (progn
            (princ (strcat "\nSkipping: " block_name " (not in allowed list)"))
            nil
          )
        )
      )
      (progn
        (princ "\nError: Not a valid block reference")
        nil
      )
    )
  )

  ;; --- Display information about matching blocks ---
  (defun display-block-info (selection_set)
    (setq i 0)
    (setq total_count (sslength selection_set))
    (setq allowed_count 0)
    
    (princ "\n=== Selected Blocks Information ===")
    
    (while (< i total_count)
      (setq block_ent (ssname selection_set i))
      (if block_ent
        (progn
          (setq block_name (get-block-name block_ent))
          (if (not block_name) (setq block_name "Anonymous"))
          
          (setq is_allowed (allowed-block-p block_name))
          (if is_allowed
            (setq allowed_count (1+ allowed_count))
          )
          
          (princ (strcat "\n" (itoa (1+ i)) ". " block_name " - " 
                        (if is_allowed "WILL PROCESS" "will SKIP")))
        )
      )
      (setq i (1+ i))
    )
    
    (princ (strcat "\n\nFound " (itoa allowed_count) " of " 
                  (itoa total_count) " blocks that will be processed"))
    (princ "\n===================================")
    
    allowed_count
  )

  ;; --- Main Routine ---
  
  ;; Initialize
  (setq old_cmdecho (getvar "cmdecho"))
  (setq old_osmode (getvar "osmode"))
  (setvar "cmdecho" 0)
  (setvar "osmode" 0)
  
  ;; Get the horizontal angle for current view
  (setq view_angle (get-view-horizontal-angle))
  
  (princ "\n=== XBURST: Block Burst for AutoCAD LT ===")
  (princ "\nWill only burst the following blocks:")
  (princ "\n  - TRAFFIC FLOW DYNA")
  (princ "\n  - PAINTED ARROW S DYNA")
  (princ "\n  - PAINTED ARROW M DYNA")
  (princ "\n  - PAINTED ARROW L DYNA")
  (princ "\n  - PAINTED ARROW XL DYNA")
  
  ;; Check for existing ARIAL text style
  (setq text_style (check-arial-style))
  
  ;; Show info about text orientation
  (princ (strcat "\nText will be created horizontal to current view (" 
                 (rtos (* 180.0 (/ view_angle pi)) 2 2) " deg)"))
  
  (princ "\nSelect blocks to burst...")
  
  ;; Get selection
  (setq selection_set (ssget))
  
  (if selection_set
    (progn
      ;; First display information about the selected blocks
      (setq allowed_count (display-block-info selection_set))
      
      (setq i 0)
      (setq success_count 0)
      (setq skipped_count 0)
      (setq total_count (sslength selection_set))
      
      (princ (strcat "\nProcessing " (itoa total_count) " block(s) with " text_style " style..."))
      
      (while (< i total_count)
        (setq block_ent (ssname selection_set i))
        
        (if block_ent
          (progn
            (if (xburst-single-block block_ent text_style view_angle)
              (setq success_count (1+ success_count))
              (setq skipped_count (1+ skipped_count))
            )
          )
          (princ "\nError: Could not access block entity")
        )
        
        (setq i (1+ i))
      )
      
      (princ (strcat "\n\n*** XBURST COMPLETE ***"))
      (princ (strcat "\nSuccessfully processed " (itoa success_count) " of " (itoa total_count) " blocks"))
      (princ (strcat "\nSkipped " (itoa skipped_count) " blocks (not in allowed list)"))
      (princ (strcat "\nAll text created with " text_style " text style"))
      (princ "\nAttributes converted to TEXT with middle center alignment, horizontal to current view")
    )
    (princ "\nNo blocks selected.")
  )
  
  ;; Restore settings
  (setvar "cmdecho" old_cmdecho)
  (setvar "osmode" old_osmode)
  (princ)
)

;;; Load message
(princ "\n--- XBurst.lsp loaded ---")
(princ "\n--- Command: XBURST ---")
(princ)