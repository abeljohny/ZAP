# ZAP

Tiny DSL in ABAP for rapid report prototyping. Featured on https://dotabap.org/explore.html.

## Installation

*Please visit https://app.assembla.com/spaces/saplink/wiki for installing the NUGG_ZAP.nugg file or simply copy the files in src/ into your system.*

## Usage

#### *SELECT Syntax*
```abap
_select '<table name> : <table fields | *> [+ <where condition1> + <where condition2> ...]' <reference variable> <reference variable | zap_null>.
```

#### *DISPLAY Syntax*
```abap
_display <reference variable>
```

#### *Example: Displaying data from table SFLIGHT*
```abap
INCLUDE zap.

START-OF-SELECTION.
  DATA: r_sflight TYPE REF TO DATA.

  _select 'sflight : carrid connid fldate price currency + carrid = ''AC'' + connid = 820' r_sflight zap_null.
  _display r_sflight.
```

#### *Example: Displaying data from table SCARR based on data from table SFLIGHT*
```abap
INCLUDE zap.

START-OF-SELECTION.
  DATA: r_sflight TYPE REF TO DATA,
        r_scarr   TYPE REF TO DATA.

  _select 'sflight : *' r_sflight zap_null.
  _select 'scarr : * + carrid = <t>-carrid' r_scarr r_sflight.
  _display r_scarr.
```


ZAP supports custom ALV fieldcatalog and layout settings for fine-grained control over report display using the fields defined in the structures slis_fieldcat_alv and slis_layout_alv.

#### *DEFINE FIELD CATALOG Syntax*
```abap
_deffcat '<field1> <field2> ...'.
```

#### *ADD FIELD CATALOG Syntax*
```abap
_addfcat '''<field1>'' ''<field2>'' ...'.
```

#### *DEFINE LAYOUT Syntax*
```abap
_deflayout '<field1> <field2> ...'.
```

#### *ADD LAYOUT Syntax*
```abap
_addlayout '''<field1>'' ''<field2>'' ...'.
```

#### *Example: Displaying data from table SFLIGHT using custom field catalog and layout*
```abap
INCLUDE ZAP.

START-OF-SELECTION.
  DATA: R_SFLIGHT TYPE REF TO DATA.

  _SELECT 'sflight : carrid connid price' R_SFLIGHT ZAP_NULL.
  _DEFFCAT 'col_pos fieldname tabname seltext_m'.
  _ADDFCAT: '''1'' ''connid'' ''sflight'' ''Flight No.''',
            '''2'' ''carrid'' ''sflight'' ''Airline''',
            '''3'' ''price'' ''sflight'' ''Price'''.
  _DEFLAYOUT 'no_vline'.
  _ADDLAYOUT '''X'''.
  _DISPLAY R_SFLIGHT.
```

## License

[GPL License 2.0](LICENSE)
