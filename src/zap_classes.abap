*&---------------------------------------------------------------------*
*&  Include           ZAP_CLASSES
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS _zap_lcl_error
*----------------------------------------------------------------------*
CLASS _ZAP_LCL_ERROR DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: DISPLAY_ERROR IMPORTING IM_ERR_STR TYPE STRING.

ENDCLASS.                    "_zap_lcl_error
*----------------------------------------------------------------------*
*       CLASS _zap_fieldcatalog
*----------------------------------------------------------------------*
CLASS _ZAP_FIELDCATALOG DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:  SET_FCAT_STRUCTURE  IMPORTING IM_FCATSTR TYPE FIELDNAME.
    CLASS-METHODS:  GET_FCAT_STRUCTURE  EXPORTING EX_FCATSTR TYPE FIELDNAME.
    CLASS-METHODS:  SET_FCAT_STRTAB     IMPORTING IM_STR     TYPE CHAR255.
    CLASS-METHODS:  GET_FCAT_STRTAB     EXPORTING FCAT_STR   TYPE ZAP_TT_CHAR255.
    CLASS-METHODS:  GET_FCAT            EXPORTING FCAT       TYPE SLIS_T_FIELDCAT_ALV.
    CLASS-METHODS:  APPEND_FCAT         IMPORTING IM_FCAT    TYPE SLIS_FIELDCAT_ALV.
  PRIVATE SECTION.
    CLASS-DATA: IT_FCAT TYPE SLIS_T_FIELDCAT_ALV.
    CLASS-DATA: IT_DD03L TYPE ZAP_TT_DD03L.
    CLASS-DATA: FCAT_STR_TAB TYPE ZAP_TT_CHAR255.
    CLASS-DATA: FCAT_ITM_TAB TYPE ZAP_TT_CHAR255.
    CLASS-DATA: FCAT_STRUCTURE TYPE TABLENAME.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_layout
*----------------------------------------------------------------------*
CLASS _ZAP_LAYOUT DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:  SET_LAYOUT_STRTAB   IMPORTING IM_STR     TYPE CHAR255.
    CLASS-METHODS:  GET_LAYOUT_STRTAB   EXPORTING LAYOUT_STR TYPE ZAP_TT_CHAR255.
    CLASS-METHODS:  GET_LAYOUT          EXPORTING EX_LAYOUT  TYPE SLIS_LAYOUT_ALV.
    CLASS-METHODS:  SET_LAYOUT          IMPORTING IM_LAYOUT  TYPE SLIS_LAYOUT_ALV.
  PRIVATE SECTION.
    CLASS-DATA: LAYOUT_STR_TAB TYPE ZAP_TT_CHAR255.
    CLASS-DATA: WA_LAYOUT TYPE SLIS_LAYOUT_ALV.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_lcl_error IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _ZAP_LCL_ERROR IMPLEMENTATION.
  METHOD DISPLAY_ERROR.
    MESSAGE IM_ERR_STR TYPE 'E' DISPLAY LIKE 'I'.
    EXIT.
  ENDMETHOD.                    "display_error

ENDCLASS.                    "_zap_lcl_error
*----------------------------------------------------------------------*
*       CLASS _zap_fieldcatalog IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _ZAP_FIELDCATALOG IMPLEMENTATION.
  METHOD SET_FCAT_STRUCTURE.
    FCAT_STRUCTURE = IM_FCATSTR.
  ENDMETHOD.
  METHOD GET_FCAT_STRUCTURE.
    EX_FCATSTR = FCAT_STRUCTURE.
  ENDMETHOD.
  METHOD SET_FCAT_STRTAB.
    SPLIT IM_STR AT SPACE INTO TABLE FCAT_STR_TAB.
  ENDMETHOD.

  METHOD GET_FCAT_STRTAB.
    FCAT_STR[] = FCAT_STR_TAB[].
  ENDMETHOD.

  METHOD GET_FCAT.
    FCAT[] = IT_FCAT[].
  ENDMETHOD.

  METHOD APPEND_FCAT.
    APPEND IM_FCAT TO IT_FCAT.
  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS _zap_layout IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS _ZAP_LAYOUT IMPLEMENTATION.
  METHOD SET_LAYOUT_STRTAB.
    SPLIT IM_STR AT SPACE INTO TABLE LAYOUT_STR_TAB.
  ENDMETHOD.

  METHOD GET_LAYOUT_STRTAB.
    LAYOUT_STR[] = LAYOUT_STR_TAB[].
  ENDMETHOD.

  METHOD GET_LAYOUT.
    MOVE-CORRESPONDING WA_LAYOUT TO EX_LAYOUT.
  ENDMETHOD.

  METHOD SET_LAYOUT.
    WA_LAYOUT = IM_LAYOUT.
  ENDMETHOD.

ENDCLASS.
