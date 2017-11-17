*&---------------------------------------------------------------------*
*&  Include           ZAP_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  _ZAP_CHECK_FIELDS
*&---------------------------------------------------------------------*
*      -->P_TABNAME  text
*      -->P_FIELDS_TAB  text
*      -->P_IT_DD03L  text
*----------------------------------------------------------------------*
FORM _ZAP_CHECK_FIELDS  USING    P_TABNAME  TYPE DD03L-TABNAME
                                 P_FIELDS_TAB TYPE ZAP_TT_FIELDS
                                 P_IT_DD03L TYPE ZAP_TT_DD03L.
  FIELD-SYMBOLS: <FS_FLD> TYPE ZAP_TY_FIELDS.
  DATA: ERR_STR TYPE STRING.

  LOOP AT P_FIELDS_TAB ASSIGNING <FS_FLD>.
    READ TABLE P_IT_DD03L WITH KEY TABNAME = P_TABNAME
                                   FIELDNAME = <FS_FLD>-FIELDNAME
                                   TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      CLEAR ERR_STR.
      CONCATENATE 'ZAPError : column' <FS_FLD> 'is unknown in table' P_TABNAME INTO ERR_STR SEPARATED BY SPACE.
      _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_WHERETAB
*&---------------------------------------------------------------------*
*      <--P__ZAP_WHERE_STR  text
*      <--P__ZAP_WHERE_TAB  text
*----------------------------------------------------------------------*
FORM _ZAP_BUILD_WHERETAB  CHANGING P__ZAP_WHERE_STR TYPE CHAR255
                              P__ZAP_WHERE_TAB TYPE ZAP_TT_CHAR255.
  IF P__ZAP_WHERE_STR IS NOT INITIAL.
    SHIFT P__ZAP_WHERE_STR LEFT DELETING LEADING SPACE.
    REPLACE FIRST OCCURRENCE OF REGEX '<[Tt]>' IN P__ZAP_WHERE_STR WITH '<FS_FAETAB>'.
    IF LINES( P__ZAP_WHERE_TAB ) > 0.
      CONCATENATE 'AND' P__ZAP_WHERE_STR INTO P__ZAP_WHERE_STR SEPARATED BY SPACE.
    ENDIF.
    APPEND P__ZAP_WHERE_STR TO P__ZAP_WHERE_TAB.
    CLEAR P__ZAP_WHERE_STR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _zap_select
*&---------------------------------------------------------------------*
FORM _ZAP_SELECT USING P_STR  TYPE CHAR255
                       P_RBSEG TYPE REF TO DATA
                       P_RFAESTR TYPE REF TO DATA.
  DATA: SEL_TAB TYPE TABLE OF STRING.
  DATA: TABNAME TYPE TABNAME.
  DATA: BEGIN OF MODE,
          MODE TYPE I,
          CTR  TYPE I,
        END OF MODE,
        CURR_MODE LIKE MODE-MODE.
  DATA: FIELDS_TAB TYPE STANDARD TABLE OF ZAP_TY_FIELDS.
  DATA: WHERE_TAB TYPE ZAP_TT_CHAR255,
        WHERE_STR TYPE CHAR255.
  DATA: FAE_TAB TYPE ZAP_TT_CHAR255.
  DATA: IT_DD03L TYPE ZAP_TT_DD03L.
  DATA: ERR_STR TYPE STRING.
  FIELD-SYMBOLS:  <FS_STR>    TYPE STRING,
                  <FS_FLD>    TYPE ZAP_TY_FIELDS,
                  <FS_FAETAB> TYPE STANDARD TABLE.
  CONSTANTS:  FIELD_MODE TYPE I VALUE 1,
              WHERE_MODE TYPE I VALUE 2.

  CONDENSE P_STR.

  SPLIT P_STR AT SPACE INTO TABLE SEL_TAB.

  LOOP AT SEL_TAB ASSIGNING <FS_STR>.
    IF SY-TABIX = 1.
      TRANSLATE <FS_STR> TO UPPER CASE.
      TABNAME = <FS_STR>.
      SELECT COUNT(*) FROM DD02L WHERE TABNAME = <FS_STR>.
      IF SY-SUBRC <> 0.
        CLEAR ERR_STR.
        ERR_STR = 'ZAPError : Parsing failed. Try separating tokens with space.'.
        _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
      ENDIF.
      _ZAP_FIELDCATALOG=>SET_FCAT_STRUCTURE( EXPORTING IM_FCATSTR = TABNAME ).
    ELSE.
      CASE <FS_STR>.
        WHEN ':'.
          CURR_MODE = FIELD_MODE.
          PERFORM _ZAP_BUILD_WHERETAB CHANGING WHERE_STR WHERE_TAB.
        WHEN '+'.
          CURR_MODE = WHERE_MODE.
          PERFORM _ZAP_BUILD_WHERETAB CHANGING WHERE_STR WHERE_TAB.
        WHEN OTHERS.
          CASE CURR_MODE.
            WHEN FIELD_MODE.
              TRANSLATE <FS_STR> TO UPPER CASE.
              APPEND <FS_STR> TO FIELDS_TAB.
            WHEN WHERE_MODE.
              FIND FIRST OCCURRENCE OF REGEX '<[Tt]>-.*' IN WHERE_STR.
              IF SY-SUBRC = 0.
                TRANSLATE <FS_STR> TO UPPER CASE.
                CONCATENATE '<FS_FAETAB>-' WHERE_STR+4 <FS_STR> INTO WHERE_STR SEPARATED BY SPACE.
              ELSE.
                CONCATENATE WHERE_STR <FS_STR> INTO WHERE_STR SEPARATED BY SPACE.
              ENDIF.
            WHEN OTHERS.
              CLEAR ERR_STR.
              ERR_STR = 'ZAPError : Parsing failed. Try separating tokens with space.'.
              _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
          ENDCASE.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  PERFORM _ZAP_BUILD_WHERETAB CHANGING WHERE_STR WHERE_TAB.

  IF P_RFAESTR <> ZAP_NULL AND LINES( WHERE_TAB ) = 0.
    CLEAR ERR_STR.
    ERR_STR = 'ZAPError : WHERE condition does not refer to the FOR ALL ENTRIES table'.
    _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
  ENDIF.

  DATA: RTABLINE TYPE REF TO CL_ABAP_STRUCTDESCR,
        TCOMP    TYPE ABAP_COMPDESCR_TAB.
  DATA: TFCAT    TYPE LVC_T_FCAT.
  DATA: TDYNTAB  TYPE REF TO DATA.
  DATA: WA_ITAB  TYPE CHAR30.
  FIELD-SYMBOLS:  <FS_DD03L>  TYPE DD03L,
                  <FS_FCAT>   TYPE LVC_S_FCAT,
                  <FS_DYNTAB> TYPE STANDARD TABLE.

  RTABLINE ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_NAME( TABNAME ).
  TCOMP = RTABLINE->COMPONENTS.

  READ TABLE FIELDS_TAB INDEX 1 ASSIGNING <FS_FLD>.

  IF SY-SUBRC = 0.
    IF <FS_FLD> = '*'.
      IF LINES( FIELDS_TAB ) <> 1.
        CLEAR ERR_STR.
        CONCATENATE 'ZAPError : field specification not allowed with * in' TABNAME 'SELECT query'
          INTO ERR_STR SEPARATED BY SPACE.
        _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
        EXIT.
      ENDIF.
      SELECT FIELDNAME
        FROM DD03L
        INTO TABLE FIELDS_TAB
        WHERE TABNAME = TABNAME
        .
    ENDIF.

    DELETE FIELDS_TAB WHERE FIELDNAME = '' OR FIELDNAME = 'MANDT'
      OR FIELDNAME CP '*.*' OR FIELDNAME CP '*/*'.

    SELECT *
      FROM DD03L
      INTO TABLE IT_DD03L
      FOR ALL ENTRIES IN FIELDS_TAB
      WHERE TABNAME = TABNAME
        AND FIELDNAME = FIELDS_TAB-FIELDNAME
      .
    PERFORM _ZAP_CHECK_FIELDS USING TABNAME FIELDS_TAB IT_DD03L.

    IF LINES( IT_DD03L ) <> LINES( FIELDS_TAB ).
      LOOP AT IT_DD03L ASSIGNING <FS_DD03L>.
        READ TABLE FIELDS_TAB WITH KEY FIELDNAME = <FS_DD03L>-FIELDNAME TRANSPORTING NO FIELDS.
        IF SY-SUBRC <> 0.
          CLEAR ERR_STR.
          CONCATENATE 'ZAPError :' <FS_DD03L>-FIELDNAME 'does not exist in' TABNAME
           INTO ERR_STR SEPARATED BY SPACE.
          _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT IT_DD03L BY POSITION.

    LOOP AT IT_DD03L ASSIGNING <FS_DD03L>.
      APPEND INITIAL LINE TO TFCAT ASSIGNING <FS_FCAT>.
      <FS_FCAT>-TABNAME   = <FS_DD03L>-TABNAME.
      <FS_FCAT>-FIELDNAME = <FS_DD03L>-FIELDNAME.
      <FS_FCAT>-DATATYPE  = <FS_DD03L>-DATATYPE.
      <FS_FCAT>-OUTPUTLEN = <FS_FCAT>-INTLEN = <FS_DD03L>-LENG.
      <FS_FCAT>-ROLLNAME  = <FS_DD03L>-ROLLNAME.
      <FS_FCAT>-DECIMALS  = <FS_DD03L>-DECIMALS.
      IF <FS_DD03L>-DATATYPE = 'CURR'.
        <FS_FCAT>-CFIELDNAME = <FS_DD03L>-FIELDNAME.
      ELSEIF <FS_DD03L>-DATATYPE = 'QUAN'.
        <FS_FCAT>-QFIELDNAME = <FS_DD03L>-FIELDNAME.
      ENDIF.
    ENDLOOP.

  ELSE.
    CLEAR ERR_STR.
    CONCATENATE 'ZAPError : no fields specified in' TABNAME 'SELECT query'
      INTO ERR_STR SEPARATED BY SPACE.
    _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
    EXIT.
  ENDIF.

  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG           = TFCAT
    IMPORTING
      EP_TABLE                  = P_RBSEG
    EXCEPTIONS
      GENERATE_SUBPOOL_DIR_FULL = 1
      OTHERS                    = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  ASSIGN P_RBSEG->* TO <FS_DYNTAB>.

  IF P_RFAESTR IS NOT INITIAL.
    ASSIGN P_RFAESTR->* TO <FS_FAETAB>.
    SELECT (FIELDS_TAB)
      FROM (TABNAME)
      INTO TABLE <FS_DYNTAB>
      FOR ALL ENTRIES IN <FS_FAETAB>
      WHERE (WHERE_TAB)
      .
  ELSE.
    SELECT (FIELDS_TAB)
      FROM (TABNAME)
      INTO TABLE <FS_DYNTAB>
      WHERE (WHERE_TAB)
      .
  ENDIF.

ENDFORM.                    "_select
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DEFFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_DEFFCAT USING P_STR TYPE CHAR255.
  CONDENSE P_STR.
  _ZAP_FIELDCATALOG=>SET_FCAT_STRTAB( P_STR ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DEFLAYOUT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_DEFLAYOUT  USING P_STR TYPE CHAR255.
  CONDENSE P_STR.
  _ZAP_LAYOUT=>SET_LAYOUT_STRTAB( P_STR ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_RESULTS_TAB
*&---------------------------------------------------------------------*
*      -->P_P_STR  text
*      <--P_RESULTS_TAB  text
*----------------------------------------------------------------------*
FORM BUILD_RESULTS_TAB  USING    P_STR TYPE CHAR255
                        CHANGING P_RESULTS_TAB TYPE MATCH_RESULT_TAB.

  FIELD-SYMBOLS: <FS_RESULT> TYPE MATCH_RESULT.

  FIND ALL OCCURRENCES OF REGEX '[^'']+' IN P_STR RESULTS P_RESULTS_TAB.

  DATA: RESULTS_LEN TYPE I.

  RESULTS_LEN = LINES( P_RESULTS_TAB ).

  LOOP AT P_RESULTS_TAB ASSIGNING <FS_RESULT>.
    IF SY-TABIX = RESULTS_LEN. EXIT. ENDIF.
    IF SY-TABIX MOD 2 = 1.
      APPEND <FS_RESULT> TO P_RESULTS_TAB.
    ENDIF.
  ENDLOOP.
  DELETE P_RESULTS_TAB FROM 1 TO RESULTS_LEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_ADDFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_ADDFCAT USING P_STR TYPE CHAR255.
  DATA: WA_FCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: RESULTS_TAB TYPE MATCH_RESULT_TAB.
  DATA: _ZAP_ERR_STR TYPE STRING.
  FIELD-SYMBOLS:  <FS_WA>     TYPE CHAR255,
                  <FS_WA2>    TYPE CHAR255,
                  <FS_RESULT> TYPE MATCH_RESULT,
                  <FS_COMP>.

  CONDENSE P_STR.

  PERFORM BUILD_RESULTS_TAB USING P_STR
                            CHANGING RESULTS_TAB.

  DATA: FCAT_ITM_TAB TYPE ZAP_TT_CHAR255.

  LOOP AT RESULTS_TAB ASSIGNING <FS_RESULT>.
    APPEND P_STR+<FS_RESULT>-OFFSET(<FS_RESULT>-LENGTH) TO FCAT_ITM_TAB.
  ENDLOOP.

  DATA: FCAT_STR_TAB TYPE ZAP_TT_CHAR255.

  _ZAP_FIELDCATALOG=>GET_FCAT_STRTAB( IMPORTING FCAT_STR = FCAT_STR_TAB ).

  DATA: ITM_LINES TYPE I,
        STR_LINES TYPE I.

  ITM_LINES = LINES( FCAT_ITM_TAB ).
  STR_LINES = LINES( FCAT_STR_TAB ).
  IF ITM_LINES <> STR_LINES.
    CLEAR _ZAP_ERR_STR.
    _ZAP_ERR_STR =  'ZAPError : Different number of args in fieldcat definition and assignment. Check whether quotes have been placed around each argument and that number of arguments is same as that defined using _deffcat'.
    _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = _ZAP_ERR_STR ).
  ENDIF.

  LOOP AT FCAT_ITM_TAB ASSIGNING <FS_WA>.
    READ TABLE FCAT_STR_TAB ASSIGNING <FS_WA2> INDEX SY-TABIX.
    ASSIGN COMPONENT <FS_WA2> OF STRUCTURE WA_FCAT TO <FS_COMP>.
    IF SY-SUBRC <> 0.
      CLEAR _ZAP_ERR_STR.
      CONCATENATE 'ZAPError : Component' <FS_WA> 'does not exist in SLIS_FIELDCAT_ALV'
        INTO _ZAP_ERR_STR SEPARATED BY SPACE.
      _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = _ZAP_ERR_STR ).
    ELSE.
      <FS_COMP> = <FS_WA>.
    ENDIF.
  ENDLOOP.

  _ZAP_FIELDCATALOG=>APPEND_FCAT( IM_FCAT = WA_FCAT ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_ADDLAYOUT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_ADDLAYOUT  USING P_STR TYPE CHAR255.
  DATA: WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA: RESULTS_TAB TYPE MATCH_RESULT_TAB.
  DATA: _ZAP_ERR_STR TYPE STRING.
  FIELD-SYMBOLS:  <FS_WA>     TYPE CHAR255,
                  <FS_WA2>    TYPE CHAR255,
                  <FS_RESULT> TYPE MATCH_RESULT,
                  <FS_COMP>.

  CONDENSE P_STR.

  PERFORM BUILD_RESULTS_TAB USING P_STR
                            CHANGING RESULTS_TAB.

  DATA: LAYOUT_ITM_TAB TYPE ZAP_TT_CHAR255.

  LOOP AT RESULTS_TAB ASSIGNING <FS_RESULT>.
    APPEND P_STR+<FS_RESULT>-OFFSET(<FS_RESULT>-LENGTH) TO LAYOUT_ITM_TAB.
  ENDLOOP.

  DATA: LAYOUT_STR_TAB TYPE ZAP_TT_CHAR255.

  _ZAP_LAYOUT=>GET_LAYOUT_STRTAB( IMPORTING LAYOUT_STR = LAYOUT_STR_TAB ).

  DATA: ITM_LINES TYPE I,
        STR_LINES TYPE I.

  ITM_LINES = LINES( LAYOUT_ITM_TAB ).
  STR_LINES = LINES( LAYOUT_STR_TAB ).
  IF ITM_LINES <> STR_LINES.
    CLEAR _ZAP_ERR_STR.
    _ZAP_ERR_STR =  'ZAPError : Different number of args in fieldcat definition and assignment'.
    _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = _ZAP_ERR_STR ).
  ENDIF.

  LOOP AT LAYOUT_ITM_TAB ASSIGNING <FS_WA>.
    READ TABLE LAYOUT_STR_TAB ASSIGNING <FS_WA2> INDEX SY-TABIX.
    ASSIGN COMPONENT <FS_WA2> OF STRUCTURE WA_LAYOUT TO <FS_COMP>.
    IF SY-SUBRC <> 0.
      CLEAR _ZAP_ERR_STR.
      CONCATENATE 'ZAPError : Component' <FS_WA> 'does not exist in SLIS_LAYOUT_ALV' INTO _ZAP_ERR_STR SEPARATED BY SPACE.
      _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = _ZAP_ERR_STR ).
    ELSE.
      <FS_COMP> = <FS_WA>.
    ENDIF.
  ENDLOOP.

  _ZAP_LAYOUT=>SET_LAYOUT( IM_LAYOUT = WA_LAYOUT ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_GETFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_GETFCAT  CHANGING    P_FCAT TYPE SLIS_T_FIELDCAT_ALV.
  _ZAP_FIELDCATALOG=>GET_FCAT( IMPORTING FCAT = P_FCAT ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_GETLAYOUT
*&---------------------------------------------------------------------*
*      <--P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_GETLAYOUT  CHANGING P_LAYOUT TYPE SLIS_LAYOUT_ALV.
  _ZAP_LAYOUT=>GET_LAYOUT( IMPORTING EX_LAYOUT = P_LAYOUT ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DISPLAY
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _ZAP_DISPLAY USING P_DATA TYPE REF TO DATA.
  DATA: IT_FCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: FCAT_STR TYPE TABNAME.
  DATA: WA_LAYOUT TYPE SLIS_LAYOUT_ALV.
  DATA: LV_TABLEDESCR TYPE REF TO CL_ABAP_TABLEDESCR,
        LV_TABLELINE  TYPE REF TO CL_ABAP_STRUCTDESCR.
  DATA: COMPONENTS_TAB TYPE ABAP_COMPDESCR_TAB.
  DATA: LIT_DD03L TYPE ZAP_TT_DD03L.
  DATA: ERR_STR TYPE STRING.
  FIELD-SYMBOLS:  <FS_DATA> TYPE STANDARD TABLE,
                  <FS_FCAT> TYPE SLIS_FIELDCAT_ALV.

  _ZAP_FIELDCATALOG=>GET_FCAT( IMPORTING FCAT = IT_FCAT ).
  _ZAP_FIELDCATALOG=>GET_FCAT_STRUCTURE( IMPORTING EX_FCATSTR = FCAT_STR ).

  IF P_DATA IS INITIAL.
    CLEAR ERR_STR.
    ERR_STR = 'ZAPError : Reference variable passed to _display is null'.
    _ZAP_LCL_ERROR=>DISPLAY_ERROR( IM_ERR_STR = ERR_STR ).
  ENDIF.

  ASSIGN P_DATA->* TO <FS_DATA>.

  _ZAP_LAYOUT=>GET_LAYOUT( IMPORTING EX_LAYOUT = WA_LAYOUT ).

  IF IT_FCAT IS INITIAL.
    DATA: R_STRUCT TYPE REF TO CL_ABAP_STRUCTDESCR.
    DATA: R_LINE   TYPE REF TO DATA.
    FIELD-SYMBOLS: <FS_COMP> TYPE ABAP_COMPDESCR.

    CREATE DATA R_LINE LIKE LINE OF <FS_DATA>.
    R_STRUCT ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA_REF( R_LINE ).

    SELECT * FROM DD03L
      INTO TABLE LIT_DD03L
      FOR ALL ENTRIES IN R_STRUCT->COMPONENTS
      WHERE TABNAME = FCAT_STR
        AND FIELDNAME = R_STRUCT->COMPONENTS-NAME
        AND KEYFLAG = 'X'
      .

    LOOP AT R_STRUCT->COMPONENTS ASSIGNING <FS_COMP>.
      APPEND INITIAL LINE TO IT_FCAT ASSIGNING <FS_FCAT>.
      <FS_FCAT>-FIELDNAME = <FS_FCAT>-REF_FIELDNAME = <FS_COMP>-NAME.
      <FS_FCAT>-TABNAME = <FS_FCAT>-REF_TABNAME = FCAT_STR.
      READ TABLE LIT_DD03L WITH KEY FIELDNAME = <FS_COMP>-NAME TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        <FS_FCAT>-KEY = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF WA_LAYOUT IS INITIAL.
    WA_LAYOUT-ZEBRA = 'X'.
    WA_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IS_LAYOUT     = WA_LAYOUT
      IT_FIELDCAT   = IT_FCAT
    TABLES
      T_OUTTAB      = <FS_DATA>
    EXCEPTIONS
      PROGRAM_ERROR = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
