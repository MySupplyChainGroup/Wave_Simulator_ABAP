FUNCTION ZSCWM_WAVE_REPLN_CREATE.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"--------------------------------------------------------------------
*  DATA: lt_main     TYPE zewm_tt_wave_errors,
*        lt_log      TYPE bapirettab,
*        lt_prod     TYPE zewmtt_wav_err_log,
*        lt_prod_qty TYPE tyt_prod_qty.
*
*  DATA: ls_main     LIKE LINE OF lt_main,
*        ls_prod     LIKE LINE OF lt_prod,
*        ls_prod_qty LIKE LINE OF lt_prod_qty.
*
*  DATA: lv_wtcreated TYPE char1,
*        lv_ans       TYPE char1.
*
*  FIELD-SYMBOLS <lfs_any> TYPE any.
*
*  LOOP AT it_data ASSIGNING <lfs_any>.
*    MOVE-CORRESPONDING <lfs_any> TO ls_main.
*    APPEND ls_main TO lt_main.
*  ENDLOOP.
*
*  IF lt_main IS INITIAL OR iv_lgnum IS INITIAL.
*    RETURN.
*  ENDIF.
*
*  "ignore lines which have WT created
**  PERFORM check_wts USING iv_lgnum CHANGING lt_main.
*
*  "Segregate case and each uom qty into two seprate fields
**  PERFORM find_case_and_ea USING iv_lgnum lt_main CHANGING lt_prod_qty.
*
*  "Create WT according to case/EA uom
**  PERFORM create_wts USING iv_lgnum CHANGING lt_prod_qty lt_log.
*
*  CLEAR: lt_prod, lv_wtcreated.
*  LOOP AT lt_prod_qty INTO ls_prod_qty.
*    CLEAR ls_prod.
*    MOVE-CORRESPONDING ls_prod_qty TO ls_prod.
*    APPEND ls_prod TO lt_prod.
*
*    IF ls_prod_qty-castanum IS NOT INITIAL OR
*       ls_prod_qty-eatanum IS NOT INITIAL.
*      lv_wtcreated = abap_true.
*    ENDIF.
*  ENDLOOP.
*
*  PERFORM update_log_db USING lt_prod space.
*
*  PERFORM display_log USING lt_log.
*
*  IF lv_wtcreated IS NOT INITIAL.
*    MESSAGE s000(00) WITH 'WTs created' DISPLAY LIKE 'S'.
*  ENDIF.





ENDFUNCTION.
