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
FORM _zap_check_fields  USING    p_tabname  TYPE dd03l-tabname
                                 p_fields_tab TYPE zap_tt_fields
                                 p_it_dd03l TYPE zap_tt_dd03l.
  FIELD-SYMBOLS: <fs_fld> TYPE zap_ty_fields.
  DATA: err_str TYPE string.

  LOOP AT p_fields_tab ASSIGNING <fs_fld>.
    READ TABLE p_it_dd03l WITH KEY tabname = p_tabname
                                   fieldname = <fs_fld>-fieldname
                                   TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CLEAR err_str.
      CONCATENATE 'ZAPError : column' <fs_fld> 'is unknown in table' p_tabname INTO err_str SEPARATED BY space.
      _zap_lcl_error=>display_error( im_err_str = err_str ).
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_WHERETAB
*&---------------------------------------------------------------------*
*      <--P__ZAP_WHERE_STR  text
*      <--P__ZAP_WHERE_TAB  text
*----------------------------------------------------------------------*
FORM _zap_build_wheretab  CHANGING p__zap_where_str TYPE char255
                              p__zap_where_tab TYPE zap_tt_char255.
  IF p__zap_where_str IS NOT INITIAL.
    SHIFT p__zap_where_str LEFT DELETING LEADING space.
    REPLACE FIRST OCCURRENCE OF REGEX '<[Tt]>' IN p__zap_where_str WITH '<FS_FAETAB>'.
    IF lines( p__zap_where_tab ) > 0.
      CONCATENATE 'AND' p__zap_where_str INTO p__zap_where_str SEPARATED BY space.
    ENDIF.
    APPEND p__zap_where_str TO p__zap_where_tab.
    CLEAR p__zap_where_str.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _zap_select
*&---------------------------------------------------------------------*
FORM _zap_select USING p_str  TYPE char255
                       p_rbseg TYPE REF TO data
                       p_rfaestr TYPE REF TO data.
  DATA: sel_tab TYPE TABLE OF string.
  DATA: tabname TYPE tabname.
  DATA: BEGIN OF mode,
          mode TYPE i,
          ctr  TYPE i,
        END OF mode,
        curr_mode LIKE mode-mode.
  DATA: fields_tab TYPE STANDARD TABLE OF zap_ty_fields.
  DATA: where_tab TYPE zap_tt_char255,
        where_str TYPE char255.
  DATA: fae_tab TYPE zap_tt_char255.
  DATA: it_dd03l TYPE zap_tt_dd03l.
  DATA: err_str TYPE string.
  FIELD-SYMBOLS: <fs_str>    TYPE string,
                 <fs_fld>    TYPE zap_ty_fields,
                 <fs_faetab> TYPE STANDARD TABLE.
  CONSTANTS: field_mode TYPE i VALUE 1,
             where_mode TYPE i VALUE 2.

  CONDENSE p_str.

  SPLIT p_str AT space INTO TABLE sel_tab.

  LOOP AT sel_tab ASSIGNING <fs_str>.
    IF sy-tabix = 1.
      TRANSLATE <fs_str> TO UPPER CASE.
      tabname = <fs_str>.
      SELECT COUNT(*) FROM dd02l WHERE tabname = <fs_str>.
      IF sy-subrc <> 0.
        CLEAR err_str.
        err_str = 'ZAPError : Parsing failed. Try separating tokens with space.'.
        _zap_lcl_error=>display_error( im_err_str = err_str ).
      ENDIF.
      _zap_fieldcatalog=>set_fcat_structure( EXPORTING im_fcatstr = tabname ).
    ELSE.
      CASE <fs_str>.
        WHEN ':'.
          curr_mode = field_mode.
          PERFORM _zap_build_wheretab CHANGING where_str where_tab.
        WHEN '+'.
          curr_mode = where_mode.
          PERFORM _zap_build_wheretab CHANGING where_str where_tab.
        WHEN OTHERS.
          CASE curr_mode.
            WHEN field_mode.
              TRANSLATE <fs_str> TO UPPER CASE.
              APPEND <fs_str> TO fields_tab.
            WHEN where_mode.
              FIND FIRST OCCURRENCE OF REGEX '<[Tt]>-.*' IN where_str.
              IF sy-subrc = 0.
                TRANSLATE <fs_str> TO UPPER CASE.
                CONCATENATE '<FS_FAETAB>-' where_str+4 <fs_str> INTO where_str SEPARATED BY space.
              ELSE.
                CONCATENATE where_str <fs_str> INTO where_str SEPARATED BY space.
              ENDIF.
            WHEN OTHERS.
              CLEAR err_str.
              err_str = 'ZAPError : Parsing failed. Try separating tokens with space.'.
              _zap_lcl_error=>display_error( im_err_str = err_str ).
          ENDCASE.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  PERFORM _zap_build_wheretab CHANGING where_str where_tab.

  IF p_rfaestr <> zap_null AND lines( where_tab ) = 0.
    CLEAR err_str.
    err_str = 'ZAPError : WHERE condition does not refer to the FOR ALL ENTRIES table'.
    _zap_lcl_error=>display_error( im_err_str = err_str ).
  ENDIF.

  DATA: rtabline TYPE REF TO cl_abap_structdescr,
        tcomp    TYPE abap_compdescr_tab.
  DATA: tfcat    TYPE lvc_t_fcat.
  DATA: tdyntab  TYPE REF TO data.
  DATA: wa_itab  TYPE char30.
  FIELD-SYMBOLS: <fs_dd03l>  TYPE dd03l,
                 <fs_fcat>   TYPE lvc_s_fcat,
                 <fs_dyntab> TYPE STANDARD TABLE.

  rtabline ?= cl_abap_typedescr=>describe_by_name( tabname ).
  tcomp = rtabline->components.

  READ TABLE fields_tab INDEX 1 ASSIGNING <fs_fld>.

  IF sy-subrc = 0.
    IF <fs_fld> = '*'.
      IF lines( fields_tab ) <> 1.
        CLEAR err_str.
        CONCATENATE 'ZAPError : field specification not allowed with * in' tabname 'SELECT query'
          INTO err_str SEPARATED BY space.
        _zap_lcl_error=>display_error( im_err_str = err_str ).
        EXIT.
      ENDIF.
      SELECT fieldname
        FROM dd03l
        INTO TABLE fields_tab
        WHERE tabname = tabname
        .
    ENDIF.

    DELETE fields_tab WHERE fieldname = '' OR fieldname = 'MANDT'
      OR fieldname CP '*.*' OR fieldname CP '*/*'.

    SELECT *
      FROM dd03l
      INTO TABLE it_dd03l
      FOR ALL ENTRIES IN fields_tab
      WHERE tabname = tabname
        AND fieldname = fields_tab-fieldname
      .
    PERFORM _zap_check_fields USING tabname fields_tab it_dd03l.

    IF lines( it_dd03l ) <> lines( fields_tab ).
      LOOP AT it_dd03l ASSIGNING <fs_dd03l>.
        READ TABLE fields_tab WITH KEY fieldname = <fs_dd03l>-fieldname TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CLEAR err_str.
          CONCATENATE 'ZAPError :' <fs_dd03l>-fieldname 'does not exist in' tabname
           INTO err_str SEPARATED BY space.
          _zap_lcl_error=>display_error( im_err_str = err_str ).
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT it_dd03l BY position.

    LOOP AT it_dd03l ASSIGNING <fs_dd03l>.
      APPEND INITIAL LINE TO tfcat ASSIGNING <fs_fcat>.
      <fs_fcat>-tabname   = <fs_dd03l>-tabname.
      <fs_fcat>-fieldname = <fs_dd03l>-fieldname.
      <fs_fcat>-datatype  = <fs_dd03l>-datatype.
      <fs_fcat>-outputlen = <fs_fcat>-intlen = <fs_dd03l>-leng.
      <fs_fcat>-rollname  = <fs_dd03l>-rollname.
      <fs_fcat>-decimals  = <fs_dd03l>-decimals.
      IF <fs_dd03l>-datatype = 'CURR'.
        <fs_fcat>-cfieldname = <fs_dd03l>-fieldname.
      ELSEIF <fs_dd03l>-datatype = 'QUAN'.
        <fs_fcat>-qfieldname = <fs_dd03l>-fieldname.
      ENDIF.
    ENDLOOP.

  ELSE.
    CLEAR err_str.
    CONCATENATE 'ZAPError : no fields specified in' tabname 'SELECT query'
      INTO err_str SEPARATED BY space.
    _zap_lcl_error=>display_error( im_err_str = err_str ).
    EXIT.
  ENDIF.

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = tfcat
    IMPORTING
      ep_table                  = p_rbseg
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ASSIGN p_rbseg->* TO <fs_dyntab>.

  IF p_rfaestr IS NOT INITIAL.
    ASSIGN p_rfaestr->* TO <fs_faetab>.
    SELECT (fields_tab)
      FROM (tabname)
      INTO TABLE <fs_dyntab>
      FOR ALL ENTRIES IN <fs_faetab>
      WHERE (where_tab)
      .
  ELSE.
    SELECT (fields_tab)
      FROM (tabname)
      INTO TABLE <fs_dyntab>
      WHERE (where_tab)
      .
  ENDIF.

ENDFORM.                    "_select
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DEFFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_deffcat USING p_str TYPE char255.
  CONDENSE p_str.
  _zap_fieldcatalog=>set_fcat_strtab( p_str ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DEFLAYOUT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_deflayout  USING p_str TYPE char255.
  CONDENSE p_str.
  _zap_layout=>set_layout_strtab( p_str ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_RESULTS_TAB
*&---------------------------------------------------------------------*
*      -->P_P_STR  text
*      <--P_RESULTS_TAB  text
*----------------------------------------------------------------------*
FORM build_results_tab  USING    p_str TYPE char255
                        CHANGING p_results_tab TYPE match_result_tab.

  FIELD-SYMBOLS: <fs_result> TYPE match_result.

  FIND ALL OCCURRENCES OF REGEX '[^'']+' IN p_str RESULTS p_results_tab.

  DATA: results_len TYPE i.

  results_len = lines( p_results_tab ).

  LOOP AT p_results_tab ASSIGNING <fs_result>.
    IF sy-tabix = results_len. EXIT. ENDIF.
    IF sy-tabix MOD 2 = 1.
      APPEND <fs_result> TO p_results_tab.
    ENDIF.
  ENDLOOP.
  DELETE p_results_tab FROM 1 TO results_len.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_ADDFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_addfcat USING p_str TYPE char255.
  DATA: wa_fcat TYPE slis_fieldcat_alv.
  DATA: results_tab TYPE match_result_tab.
  DATA: _zap_err_str TYPE string.
  FIELD-SYMBOLS: <fs_wa>     TYPE char255,
                 <fs_wa2>    TYPE char255,
                 <fs_result> TYPE match_result,
                 <fs_comp>.

  CONDENSE p_str.

  PERFORM build_results_tab USING p_str
                            CHANGING results_tab.

  DATA: fcat_itm_tab TYPE zap_tt_char255.

  LOOP AT results_tab ASSIGNING <fs_result>.
    APPEND p_str+<fs_result>-offset(<fs_result>-length) TO fcat_itm_tab.
  ENDLOOP.

  DATA: fcat_str_tab TYPE zap_tt_char255.

  _zap_fieldcatalog=>get_fcat_strtab( IMPORTING fcat_str = fcat_str_tab ).

  DATA: itm_lines TYPE i,
        str_lines TYPE i.

  itm_lines = lines( fcat_itm_tab ).
  str_lines = lines( fcat_str_tab ).
  IF itm_lines <> str_lines.
    CLEAR _zap_err_str.
    _zap_err_str =  'ZAPError : Different number of args in fieldcat definition and assignment. Check whether quotes have been placed around each argument and that number of arguments is same as that defined using _deffcat'.
    _zap_lcl_error=>display_error( im_err_str = _zap_err_str ).
  ENDIF.

  LOOP AT fcat_itm_tab ASSIGNING <fs_wa>.
    READ TABLE fcat_str_tab ASSIGNING <fs_wa2> INDEX sy-tabix.
    ASSIGN COMPONENT <fs_wa2> OF STRUCTURE wa_fcat TO <fs_comp>.
    IF sy-subrc <> 0.
      CLEAR _zap_err_str.
      CONCATENATE 'ZAPError : Component' <fs_wa> 'does not exist in SLIS_FIELDCAT_ALV'
        INTO _zap_err_str SEPARATED BY space.
      _zap_lcl_error=>display_error( im_err_str = _zap_err_str ).
    ELSE.
      <fs_comp> = <fs_wa>.
    ENDIF.
  ENDLOOP.

  _zap_fieldcatalog=>append_fcat( im_fcat = wa_fcat ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_ADDLAYOUT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_addlayout  USING p_str TYPE char255.
  DATA: wa_layout TYPE slis_layout_alv.
  DATA: results_tab TYPE match_result_tab.
  DATA: _zap_err_str TYPE string.
  FIELD-SYMBOLS: <fs_wa>     TYPE char255,
                 <fs_wa2>    TYPE char255,
                 <fs_result> TYPE match_result,
                 <fs_comp>.

  CONDENSE p_str.

  PERFORM build_results_tab USING p_str
                            CHANGING results_tab.

  DATA: layout_itm_tab TYPE zap_tt_char255.

  LOOP AT results_tab ASSIGNING <fs_result>.
    APPEND p_str+<fs_result>-offset(<fs_result>-length) TO layout_itm_tab.
  ENDLOOP.

  DATA: layout_str_tab TYPE zap_tt_char255.

  _zap_layout=>get_layout_strtab( IMPORTING layout_str = layout_str_tab ).

  DATA: itm_lines TYPE i,
        str_lines TYPE i.

  itm_lines = lines( layout_itm_tab ).
  str_lines = lines( layout_str_tab ).
  IF itm_lines <> str_lines.
    CLEAR _zap_err_str.
    _zap_err_str =  'ZAPError : Different number of args in fieldcat definition and assignment'.
    _zap_lcl_error=>display_error( im_err_str = _zap_err_str ).
  ENDIF.

  LOOP AT layout_itm_tab ASSIGNING <fs_wa>.
    READ TABLE layout_str_tab ASSIGNING <fs_wa2> INDEX sy-tabix.
    ASSIGN COMPONENT <fs_wa2> OF STRUCTURE wa_layout TO <fs_comp>.
    IF sy-subrc <> 0.
      CLEAR _zap_err_str.
      CONCATENATE 'ZAPError : Component' <fs_wa> 'does not exist in SLIS_LAYOUT_ALV' INTO _zap_err_str SEPARATED BY space.
      _zap_lcl_error=>display_error( im_err_str = _zap_err_str ).
    ELSE.
      <fs_comp> = <fs_wa>.
    ENDIF.
  ENDLOOP.

  _zap_layout=>set_layout( im_layout = wa_layout ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_GETFCAT
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_getfcat  CHANGING    p_fcat TYPE slis_t_fieldcat_alv.
  _zap_fieldcatalog=>get_fcat( IMPORTING fcat = p_fcat ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_GETLAYOUT
*&---------------------------------------------------------------------*
*      <--P_&1  text
*----------------------------------------------------------------------*
FORM _zap_getlayout  CHANGING p_layout TYPE slis_layout_alv.
  _zap_layout=>get_layout( IMPORTING ex_layout = p_layout ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  _ZAP_DISPLAY
*&---------------------------------------------------------------------*
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM _zap_display USING p_data TYPE REF TO data.
  DATA: it_fcat TYPE slis_t_fieldcat_alv.
  DATA: fcat_str TYPE tabname.
  DATA: wa_layout TYPE slis_layout_alv.
  DATA: lv_tabledescr TYPE REF TO cl_abap_tabledescr,
        lv_tableline  TYPE REF TO cl_abap_structdescr.
  DATA: components_tab TYPE abap_compdescr_tab.
  DATA: lit_dd03l TYPE zap_tt_dd03l.
  DATA: err_str TYPE string.
  FIELD-SYMBOLS: <fs_data> TYPE STANDARD TABLE,
                 <fs_fcat> TYPE slis_fieldcat_alv.

  _zap_fieldcatalog=>get_fcat( IMPORTING fcat = it_fcat ).
  _zap_fieldcatalog=>get_fcat_structure( IMPORTING ex_fcatstr = fcat_str ).

  IF p_data IS INITIAL.
    CLEAR err_str.
    err_str = 'ZAPError : Reference variable passed to _display is null'.
    _zap_lcl_error=>display_error( im_err_str = err_str ).
  ENDIF.

  ASSIGN p_data->* TO <fs_data>.

  _zap_layout=>get_layout( IMPORTING ex_layout = wa_layout ).

  IF it_fcat IS INITIAL.
    DATA: r_struct TYPE REF TO cl_abap_structdescr.
    DATA: r_line   TYPE REF TO data.
    FIELD-SYMBOLS: <fs_comp> TYPE abap_compdescr.

    CREATE DATA r_line LIKE LINE OF <fs_data>.
    r_struct ?= cl_abap_typedescr=>describe_by_data_ref( r_line ).

    SELECT * FROM dd03l
      INTO TABLE lit_dd03l
      FOR ALL ENTRIES IN r_struct->components
      WHERE tabname = fcat_str
        AND fieldname = r_struct->components-name
        AND keyflag = 'X'
      .

    LOOP AT r_struct->components ASSIGNING <fs_comp>.
      APPEND INITIAL LINE TO it_fcat ASSIGNING <fs_fcat>.
      <fs_fcat>-fieldname = <fs_fcat>-ref_fieldname = <fs_comp>-name.
      <fs_fcat>-tabname = <fs_fcat>-ref_tabname = fcat_str.
      READ TABLE lit_dd03l WITH KEY fieldname = <fs_comp>-name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <fs_fcat>-key = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wa_layout IS INITIAL.
    wa_layout-zebra = 'X'.
    wa_layout-colwidth_optimize = 'X'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      is_layout     = wa_layout
      it_fieldcat   = it_fcat
    TABLES
      t_outtab      = <fs_data>
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
