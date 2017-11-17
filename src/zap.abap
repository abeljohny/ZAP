*&---------------------------------------------------------------------*
*&  Include           ZAP
*&---------------------------------------------------------------------*

TYPE-POOLS: ZAP, ABAP.

INCLUDE ZAP_CLASSES.
INCLUDE ZAP_FORMS.

*----------------------------------------------------------------------*
* DESCRIPTION  : short hand Open SQL SELECT Statement
* SYNTAX       :  _select '<table name> : <field1> <field2> ... + <where cond1> + <where cond2>' <ref. var> <fae ref. var>
*                (for all entries table is expressed as <t>)
* USAGE        : _select 'bseg + bukrs = ''PEI'' + belnr = ''1800003603'' : bukrs belnr' R_BSEG NULL
*                _select 'bseg : bukrs + bukrs = ''PEI'' + belnr = <t>-belnr' R_BSEG2 R_BSEG.
* NOTE         : all tokens must be seperated by space and are case insensitive
*              : Operators + (WHERE clause) and : (field specification) can be specified in any order
*----------------------------------------------------------------------*
DEFINE _SELECT.
  PERFORM _zap_select USING &1 &2 &3.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : defines the signature of internal fieldcatalog subroutine.
*               _addfcat will then work using this signature.
* SYNTAX       :  _deffcat '<field1> <field2> ...'.
*              :  <field1> <field2> ... are fields of SLIS_FIELDCAT_ALV
* USAGE        : _deffcat 'fieldname tabname seltext_s'.
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _DEFFCAT.
  PERFORM _zap_deffcat USING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : creates internal fieldcatalog based on signature defined using _deffcat
* SYNTAX       :  _addfcat '<field1> <field2> ...'.
*              :  where <field1> <field2> ... are field values to internal fieldcatalog
* USAGE        : _addfcat '''BUKRS''    ''BSEG'' ''Company''.
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _ADDFCAT.
  PERFORM _zap_addfcat USING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : returns internal fieldcatalog created using _deffcat and _addfcat
* SYNTAX       :  _getfcat <field>
*              :  where <field> is typed to SLIS_T_FIELDCAT_ALV
* USAGE        : DATA:  lit_fcat TYPE slis_t_fieldcat_alv.
*              : _getfcat lit_fcat.
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _GETFCAT.
  PERFORM _zap_getfcat CHANGING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : defines signature for internal layout structure for use in ALV
* SYNTAX       :  _deflayout '<field1> <field2> ...'
*              :  where <field1> <field2> ... are fields of SLIS_LAYOUT_ALV
* USAGE        : _deflayout 'zebra'.
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _DEFLAYOUT.
  PERFORM _zap_deflayout USING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : creates internal layout structure based on signature defined using _deflayout
* SYNTAX       :  _addlayout '<field1> <field2> ...'.
*              :  where <field1> <field2> ... are field values to internal layout structure
* USAGE        : _addlayout '''X'''.
* NOTE         : all tokens must be seperated by space and enclosed in single quotes
*----------------------------------------------------------------------*
DEFINE _ADDLAYOUT.
  PERFORM _zap_addlayout USING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : returns internal layout structure created using _deflayout and _addlayout
* SYNTAX       :  _getlayout <field>
*              :  where <field> is typed to SLIS_FIELDCAT_LAYOUT_ALV
* USAGE        : DATA:  lwa_layout TYPE slis_layout_alv.
*              : _getlayout lwa_layout.
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _GETLAYOUT.
  PERFORM _zap_getlayout CHANGING &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* DESCRIPTION  : displays ALV Grid based on fieldcatalog (optional) and layout (optional)
*              : if fieldcatalog is not specified, one is built internally
*              : based on the structure of the data table
* SYNTAX       :  _display <variable>
*              :  where <variable> is a data reference variable of the form returned by _select
* NOTE         : all tokens must be seperated by space and are case insensitive
*----------------------------------------------------------------------*
DEFINE _DISPLAY.
  PERFORM _zap_display USING &1.
END-OF-DEFINITION.
