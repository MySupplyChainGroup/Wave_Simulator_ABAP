FUNCTION-POOL zscwm_wave_monitor.           "MESSAGE-ID ..

* INCLUDE LZSCWM_WAVE_MONITORD...            " Local class definition

TYPE-POOLS: wmegc.


TABLES: /scwm/s_wavehdr_lgnum,
        zewm_wav_err_log.


TYPES: BEGIN OF ty_simulate_buffer,
         lgnum TYPE /scwm/lgnum,
         wave  TYPE /scwm/de_wave,
         type  TYPE symsgty,
       END OF ty_simulate_buffer.


TYPES: BEGIN OF ty_prod_qty.
    INCLUDE TYPE zewm_wave_errors.

TYPES: case_qty   TYPE /scwm/de_ui_wvquan,
       each_qty   TYPE /scwm/de_ui_wvquan,
       nopackspec TYPE char1,
       castanum   TYPE /scwm/tanum,
       eatanum    TYPE /scwm/tanum,
       END OF ty_prod_qty.

TYPES: BEGIN OF ty_whr_open,
         docno  TYPE /scdl/dl_docno_int,
         itemno TYPE /scwm/de_itemno_r,
         opnq   TYPE /scwm/de_opnq_b,
         uom    TYPE /scwm/de_base_uom,
       END OF ty_whr_open.

TYPES: BEGIN OF ty_whr_status,
         docno        TYPE /scdl/dl_docno_int,
         itemno       TYPE /scwm/de_itemno_r,
         status_type  TYPE /scdl/dl_status_type,
         status_value TYPE /scdl/dl_status_value,
       END OF ty_whr_status.

TYPES: BEGIN OF ty_docid_open,
*         wave     TYPE /scwm/de_wave,
*         wave_itm TYPE /scwm/de_wave_itm,
         docat TYPE /scdl/dl_doccat,
         docid TYPE /scdl/dl_docid,
         itmid TYPE /scwm/de_itmid,
         total TYPE /scdl/dl_quantity,
         opnq  TYPE /scdl/dl_quantity,
         uom   TYPE /scwm/de_base_uom,
       END OF ty_docid_open.

TYPES: BEGIN OF ty_range,
         fieldname  TYPE lvc_fname,
         start_date TYPE sy-datum,
         end_date   TYPE sy-datum,
       END OF ty_range.

TYPES: tyt_docid_open TYPE TABLE OF ty_docid_open.

TYPES: tyt_prod_qty   TYPE TABLE OF ty_prod_qty,
       tyt_whr_open   TYPE TABLE OF ty_whr_open,
       tyt_whr_status TYPE TABLE OF ty_whr_status.

CONSTANTS :gc_icon_red    TYPE iconname VALUE 'ICON_RED_LIGHT',
           gc_icon_yellow TYPE iconname VALUE 'ICON_YELLOW_LIGHT',
           gc_icon_green  TYPE iconname VALUE 'ICON_GREEN_LIGHT',
           gc_error(6)    VALUE 'Error',
           gc_success(6)  VALUE 'Success',
           gc_warning(15) VALUE 'Not Simulated'.

DATA: gt_simulate_buffer    TYPE STANDARD TABLE OF ty_simulate_buffer,
      gv_simulation_obl_off TYPE xfeld,
      gv_simulate_checked   TYPE xfeld,
      gt_wave_errors type ZEWM_TT_WAVE_ERRORS.


DATA: fcode TYPE okcode.

DATA: gv_change_data TYPE xfeld,
      gv_1st_step    TYPE xfeld,
      gv_2nd_step    TYPE xfeld.

DATA: gv_2st_direct  TYPE /scwm/de_2st_direct,
      gv_2st_distr   TYPE /scwm/de_2st_distr,
      gv_2st_removal TYPE /scwm/de_2st_removal.

*Log Data
DATA: go_log      TYPE REF TO /scwm/cl_log,
      go_tm_trace TYPE REF TO /scwm/if_tm_trace,
      gv_msgtext  TYPE bapi_msg.                            "#EC NEEDED

DATA: lv_product  TYPE /scdl/dl_productno,
      lv_wave     TYPE /scwm/de_wave,
      lv_dat_from TYPE /scwm/de_started_date,
      lv_dat_to   TYPE /scwm/de_started_date.

CONSTANTS: gc_full_repln    TYPE char1 VALUE 'F',
           gc_dynamic_repln TYPE char1 VALUE 'D'.

"Selection screen

SELECTION-SCREEN BEGIN OF SCREEN 0100 AS WINDOW.

SELECTION-SCREEN BEGIN OF BLOCK frm000 WITH FRAME TITLE TEXT-006.

SELECT-OPTIONS so_wave FOR /scwm/s_wavehdr_lgnum-wave.

SELECT-OPTIONS so_prod FOR zewm_wav_err_log-productno.

SELECTION-SCREEN:
BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(31) TEXT-007 FOR FIELD p_stdf.

PARAMETERS:
p_stdf TYPE /scwm/de_started_date.

SELECTION-SCREEN COMMENT 52(5) TEXT-008 FOR FIELD p_stdt.

PARAMETERS:
p_stdt TYPE /scwm/de_started_date.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

PARAMETERS: p_cas TYPE char1 RADIOBUTTON GROUP b1,
            p_eac TYPE char1 RADIOBUTTON GROUP b1.

SELECTION-SCREEN END OF BLOCK frm000.

SELECTION-SCREEN END OF SCREEN 100.
