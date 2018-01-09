class ZCL_MAIL definition
  public
  abstract
  create public .

  "GLOBAL FRIENDS zcl_mail_demo.
public section.

  class-data GO_UNIQUE_INSTANCE type ref to ZCL_MAIL .

  methods CONSTRUCTOR
    raising
      CX_SEND_REQ_BCS .
protected section.

  methods BUILD_BODY
  abstract
    returning
      value(RV_BODY) type STRING .
  methods BUILD_ATTACHMENTS
  abstract
    returning
      value(RT_ATTACHMENTS) type FILE_ATTRIBUTES_T .
  methods BUILD_RECIPIENTS
  abstract
    returning
      value(RT_RECIPIENTS) type UWS_T_RECIPIENTS .
  methods SEND
    returning
      value(RV_SENT_TO_ALL) type OS_BOOLEAN
    raising
      CX_DOCUMENT_BCS
      CX_ADDRESS_BCS
      CX_SEND_REQ_BCS .
  methods SET_SENDER
    importing
      !IV_SENDER_UNAME type UNAME default SY-UNAME .
  methods SET_LANGUAGE
    importing
      !IV_LANGU type LANGU default SY-LANGU .
  methods BUILD_SUBJECT
  abstract
    returning
      value(RV_SUBJECT) type STRING .
  methods BUILD_TITLE
  abstract
    returning
      value(RV_TITLE) type STRING .
  methods BUILD_SIGNATURE
  abstract
    returning
      value(RV_SIGNATURE) type STRING .
  methods READ_TEXT
    importing
      !IV_TEXT type TDOBNAME
      !IV_LANGU type LANGU optional
      !IT_TRANSCO type ZMAIL_CONVERT_VAR_T optional
    returning
      value(RV_TEXT) type STRING .
  class-methods GENERATE_BUTTON
    importing
      !IV_CLASS type STRING
      !IV_TEXT type STRING
      !IV_HREF type STRING
    returning
      value(RV_BUTTON) type STRING .
  class-methods ITAB_TO_HTML
    importing
      !IT_FCAT type LVC_T_FCAT
      !IT_DATA type ref to DATA .
private section.

  data GV_DOC_TYP type SO_OBJ_TYP value 'HTM' ##NO_TEXT.
  data GV_SENDER_UNAME type UNAME .
  data GV_LANGU type LANGU .
  data GO_SEND_REQUEST type ref to CL_BCS .

  methods ADD_ATTACHMENTS
    importing
      !IO_DOCUMENT type ref to CL_DOCUMENT_BCS
    exceptions
      INVALID_ATTACHMENT .
  methods ADD_RECIPIENTS
    raising
      CX_ADDRESS_BCS
      CX_SEND_REQ_BCS .
  methods BUILD_CONTENT
    returning
      value(RV_CONTENT) type STRING .
  methods INIT_DOC_TYP
    returning
      value(RV_DOC_TYP) type SO_OBJ_TP .
  methods BUILD_FOOTER
    returning
      value(RV_FOOTER) type STRING .
  methods BUILD_HEADER
    returning
      value(RV_HEADER) type STRING .
  methods ALERT_HOTLINE .
ENDCLASS.



CLASS ZCL_MAIL IMPLEMENTATION.


  METHOD add_attachments.
    DATA : lt_attachments TYPE file_attributes_t,
           ls_attachment  TYPE file_attributes_s.

    " Get attachments
    lt_attachments = build_attachments( ).

    " Add attachments to mail
    LOOP AT  lt_attachments INTO ls_attachment.
      TRY.
          io_document->add_attachment(
                             i_attachment_type    = ls_attachment-iv_file_type
                             i_attachment_size    = ls_attachment-iv_file_size
                             i_attachment_subject = ls_attachment-iv_file_subject
                             i_att_content_hex    = ls_attachment-iv_file_content_hex ).
        CATCH cx_document_bcs.
          " ERROR HANDLING
          RAISE invalid_attachment.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_recipients.
    DATA : lt_recipients TYPE uws_t_recipients,
           ls_recipient  TYPE uws_s_recipients,
           lv_smtp_addr  TYPE ad_smtpadr,
           lo_recipient  TYPE REF TO if_recipient_bcs,
           ls_param      TYPE uws_s_service_parameter,
           lv_copy       TYPE os_boolean,
           lv_blind_copy TYPE os_boolean,
           lv_express    TYPE os_boolean,
           lv_no_fwd     TYPE os_boolean.

    " Set recipients
    lt_recipients = build_recipients( ).

    " Add recipients to the send request
    LOOP AT lt_recipients INTO ls_recipient.
      IF ls_recipient-bname IS NOT INITIAL.
        lo_recipient = cl_sapuser_bcs=>create( ls_recipient-bname ).
      ELSEIF ls_recipient-bpartner IS NOT INITIAL.
        " NOT HANDLED IN THIS VERSION !
      ELSEIF ls_recipient-email_addr IS NOT INITIAL.
        lv_smtp_addr = ls_recipient-email_addr.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( i_address_string = lv_smtp_addr ).
      ENDIF.

      " Express ?
      READ TABLE ls_recipient-custom_attributes INTO ls_param WITH KEY name = 'EXPRESS' TRANSPORTING value.
      IF sy-subrc EQ 0.
        lv_express = ls_param-value.
      ENDIF.

      " Copy ?
      READ TABLE ls_recipient-custom_attributes INTO ls_param WITH KEY name = 'COPY' TRANSPORTING value.
      IF sy-subrc EQ 0.
        lv_copy = ls_param-value.
      ENDIF.

      " Blind copy ?
      READ TABLE ls_recipient-custom_attributes INTO ls_param WITH KEY name = 'BLIND_COPY' TRANSPORTING value.
      IF sy-subrc EQ 0.
        lv_blind_copy = ls_param-value.
      ENDIF.

      " No forward ?
      READ TABLE ls_recipient-custom_attributes INTO ls_param WITH KEY name = 'NO_FWD' TRANSPORTING value.
      IF sy-subrc EQ 0.
        lv_no_fwd = ls_param-value.
      ENDIF.

      " Add recipient
      me->go_send_request->add_recipient(
                                i_recipient = lo_recipient
                                i_express = lv_express
                                i_copy = lv_copy
                                i_blind_copy = lv_blind_copy
                                i_no_forward = lv_no_fwd ).
    ENDLOOP.
  ENDMETHOD.


  METHOD alert_hotline.

  ENDMETHOD.


  METHOD build_content.
    " Mail content
    rv_content = build_header( ) && build_body( ) && build_footer( ).
  ENDMETHOD.


  METHOD build_footer.
    rv_footer = build_signature( ) && '</body></html>'. "read_text( iv_text = 'MAIL_FOOTER_TECH').
  ENDMETHOD.


  METHOD build_header.
    rv_header = read_text( iv_text = 'MAIL_HEADER_TECH' iv_langu = 'F') && build_title( ).
  ENDMETHOD.


  METHOD constructor.
    " Init send request
    go_send_request = cl_bcs=>create_persistent( ).
  ENDMETHOD.


  METHOD generate_button.
    " Init button
    rv_button = '<a href=`' && iv_href && '` class=`button ' && iv_class && '`>' && iv_text && '</a>'.
  ENDMETHOD.


  METHOD init_doc_typ.
    rv_doc_typ = 'HTM'. "#EC_NEEDED
  ENDMETHOD.


  METHOD itab_to_html.
    DATA: ls_fcat   TYPE lvc_s_fcat,
          lt_header TYPE STANDARD TABLE OF w3head,   "Header
          lt_field  TYPE STANDARD TABLE OF w3fields, "Fields
          lt_html   TYPE STANDARD TABLE OF w3html,   "Html
          ls_head   TYPE w3head.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
    ASSIGN it_data->* TO <lt_data>.

    " Fill the Column headings and Properties
    LOOP AT it_fcat INTO ls_fcat.
      "--> Init column name
      ls_head-text = ls_fcat-coltext.

      "--> Populate the Column Headings
      CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
        EXPORTING
          field_nr = sy-tabix
          text     = ls_head-text
          fgcolor  = 'black'
          bgcolor  = 'blue'
        TABLES
          header   = lt_header.

      "--> Populate Column Properties
      CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
        EXPORTING
          field_nr = sy-tabix
          fgcolor  = 'black'
          size     = '3'
        TABLES
          fields   = lt_field.
    ENDLOOP.

    " Preparing the HTML from Intenal Table
    REFRESH lt_html.
    CALL FUNCTION 'WWW_ITAB_TO_HTML'
      TABLES
        html       = lt_html
        fields     = lt_field
        row_header = lt_header
        itable     = <lt_data>.
  ENDMETHOD.


  METHOD read_text.
    DATA : lt_lines   TYPE TABLE OF tline,
           ls_line    TYPE tline,
           ls_soli    TYPE soli,
           lv_string  TYPE string,
           ls_transco TYPE zmail_convert_var_s,
           lv_langu   TYPE langu.

    " Language determination
    " --> IV_LANGU > GV_LANGU > SY-LANGU
    IF iv_langu IS NOT SUPPLIED.
      IF gv_langu IS NOT INITIAL.
        lv_langu = gv_langu.
      ELSE.
        lv_langu = sy-langu.
      ENDIF.
    ELSE.
      lv_langu = iv_langu.
    ENDIF.

    "  Read text
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = lv_langu
        name                    = iv_text
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
*      implement suitable error handling here
    ENDIF.

    " Conc. text into string
    LOOP AT lt_lines INTO ls_line.
      CONCATENATE lv_string ls_line-tdline INTO lv_string.
    ENDLOOP.

    " Apply replacements
    LOOP AT it_transco INTO ls_transco.
      REPLACE ALL OCCURRENCES OF ls_transco-var IN lv_string WITH ls_transco-value.
    ENDLOOP.

    " Return string
    rv_text = lv_string.
  ENDMETHOD.


  METHOD send.
    " Init sender
    set_sender( ).

    " Init language
    set_language( ).

    " Add recipients
    add_recipients( ).

    " Init sender - current user by default
    go_send_request->set_sender( cl_sapuser_bcs=>create( gv_sender_uname ) ).

    " Init subject
    DATA(lv_subject) = build_subject( ) .

    " Create document
    DATA(lo_document) = cl_document_bcs=>create_document(
                            i_type      = gv_doc_typ
                            i_language  = gv_langu
                            i_subject   = CONV so_obj_des( lv_subject )
                            i_text      = cl_document_bcs=>string_to_soli( build_content( ) ) ).
    " Set long subject
    go_send_request->set_message_subject( ip_subject = lv_subject ).

    " Add attachments
    add_attachments( lo_document ).

    " Assign document to the send request
    go_send_request->set_document( lo_document ).

    " Send immediately
    go_send_request->set_send_immediately( abap_true ).

    " Send email
    rv_sent_to_all  = go_send_request->send( ).
  ENDMETHOD.


  METHOD set_language.
**********************************************************************
* DO NOT CALL THIS METHOD DIRECTLY !                                 *
* OVERWRITE THIS METHOD                                              *
* CALL SUPER->SET_CONSTANTS TO CHANGE VARIABLES USING PARAMETERS     *
**********************************************************************
    gv_langu        = iv_langu.
  ENDMETHOD.


  METHOD set_sender.
**********************************************************************
* DO NOT CALL THIS METHOD DIRECTLY !                                 *
* OVERWRITE THIS METHOD                                              *
* CALL SUPER->SET_CONSTANTS TO CHANGE VARIABLES USING PARAMETERS     *
**********************************************************************
    gv_sender_uname = iv_sender_uname.
  ENDMETHOD.
ENDCLASS.
