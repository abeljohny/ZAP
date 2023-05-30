*&---------------------------------------------------------------------*
*&  Include           ZAP_CLASSES
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS _zap_lcl_error
*----------------------------------------------------------------------*
CLASS _zap_lcl_error DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: display_error IMPORTING im_err_str TYPE string.

ENDCLASS.                    "_zap_lcl_error
*----------------------------------------------------------------------*
*       CLASS _zap_fieldcatalog
*----------------------------------------------------------------------*
CLASS _zap_fieldcatalog DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:  set_fcat_structure  IMPORTING im_fcatstr TYPE fieldname.
    CLASS-METHODS:  get_fcat_structure  EXPORTING ex_fcatstr TYPE fieldname.
    CLASS-METHODS:  set_fcat_strtab     IMPORTING im_str     TYPE char255.
    CLASS-METHODS:  get_fcat_strtab     EXPORTING fcat_str   TYPE zap_tt_char255.
    CLASS-METHODS:  get_fcat            EXPORTING fcat       TYPE slis_t_fieldcat_alv.
    CLASS-METHODS:  append_fcat         IMPORTING im_fcat    TYPE slis_fieldcat_alv.
  PRIVATE SECTION.
    CLASS-DATA: it_fcat TYPE slis_t_fieldcat_alv.
    CLASS-DATA: it_dd03l TYPE zap_tt_dd03l.
    CLASS-DATA: fcat_str_tab TYPE zap_tt_char255.
    CLASS-DATA: fcat_itm_tab TYPE zap_tt_char255.
    CLASS-DATA: fcat_structure TYPE tablename.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_layout
*----------------------------------------------------------------------*
CLASS _zap_layout DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:  set_layout_strtab   IMPORTING im_str     TYPE char255.
    CLASS-METHODS:  get_layout_strtab   EXPORTING layout_str TYPE zap_tt_char255.
    CLASS-METHODS:  get_layout          EXPORTING ex_layout  TYPE slis_layout_alv.
    CLASS-METHODS:  set_layout          IMPORTING im_layout  TYPE slis_layout_alv.
  PRIVATE SECTION.
    CLASS-DATA: layout_str_tab TYPE zap_tt_char255.
    CLASS-DATA: wa_layout TYPE slis_layout_alv.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_lcl_error IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _zap_lcl_error IMPLEMENTATION.
  METHOD display_error.
    MESSAGE im_err_str TYPE 'E' DISPLAY LIKE 'I'.
    EXIT.
  ENDMETHOD.                    "display_error

ENDCLASS.                    "_zap_lcl_error
*----------------------------------------------------------------------*
*       CLASS _zap_fieldcatalog IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _zap_fieldcatalog IMPLEMENTATION.
  METHOD set_fcat_structure.
    fcat_structure = im_fcatstr.
  ENDMETHOD.
  METHOD get_fcat_structure.
    ex_fcatstr = fcat_structure.
  ENDMETHOD.
  METHOD set_fcat_strtab.
    SPLIT im_str AT space INTO TABLE fcat_str_tab.
  ENDMETHOD.

  METHOD get_fcat_strtab.
    fcat_str[] = fcat_str_tab[].
  ENDMETHOD.

  METHOD get_fcat.
    fcat[] = it_fcat[].
  ENDMETHOD.

  METHOD append_fcat.
    APPEND im_fcat TO it_fcat.
  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_layout IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _zap_layout IMPLEMENTATION.
  METHOD set_layout_strtab.
    SPLIT im_str AT space INTO TABLE layout_str_tab.
  ENDMETHOD.

  METHOD get_layout_strtab.
    layout_str[] = layout_str_tab[].
  ENDMETHOD.

  METHOD get_layout.
    MOVE-CORRESPONDING wa_layout TO ex_layout.
  ENDMETHOD.

  METHOD set_layout.
    wa_layout = im_layout.
  ENDMETHOD.

ENDCLASS.
