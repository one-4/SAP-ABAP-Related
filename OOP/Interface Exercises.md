# **INTERFACES IN SAP ABAP**  

---

# **TABLE OF CONTENTS**

## [EXERCISES AND SOLUTIONS](#exercises-and-solutions)

### [Practical Exercises with Solutions](#practical-exercises-with-solutions)
  - [Exercise 1: Basic Interface Implementation](#exercise-1-basic-interface-implementation)
    - Problem Statement
    - Solution
    
  - [Exercise 2: Multiple Interfaces](#exercise-2-multiple-interfaces)
    - Problem Statement
    - Solution
    
  - [Exercise 3: Interface with Events](#exercise-3-interface-with-events)
    - Problem Statement
    - Solution
    
  - [Exercise 4: Compound Interfaces](#exercise-4-compound-interfaces)
    - Problem Statement
    - Solution
    
  - [Exercise 5: Real-World Banking System](#exercise-5-real-world-banking-system)
    - Problem Statement
    - Solution

### [Complex Real-World Problems](#complex-real-world-problems)

### [Chapter Summary](#chapter-summary)

### [Final Challenge: Complete E-Commerce System](#final-challenge-complete-e-commerce-system)

---


## **Exercises and Solutions**

---

### **Practical Exercises with Solutions**

#### **Exercise 1: Basic Interface Implementation**
**Problem Statement:**  
Create an interface `IF_VEHICLE` with methods `start_engine()`, `stop_engine()`, and `get_fuel_level()`. Implement this interface in two classes: `LCL_CAR` and `LCL_MOTORCYCLE`. Each class should have different implementations of the methods.

**Solution:**
```abap
*&---------------------------------------------------------------------*
*& Exercise 1 Solution: Basic Interface Implementation
*&---------------------------------------------------------------------*
REPORT z_exercise_1_solution.

*----------------------------------------------------------------------*
* INTERFACE: IF_VEHICLE
*----------------------------------------------------------------------*
INTERFACE if_vehicle.
  METHODS:
    start_engine RETURNING VALUE(rv_status) TYPE string,
    stop_engine  RETURNING VALUE(rv_status) TYPE string,
    get_fuel_level RETURNING VALUE(rv_fuel) TYPE i.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: LCL_CAR
*----------------------------------------------------------------------*
CLASS lcl_car DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_vehicle.
    ALIASES:
      start_engine  FOR if_vehicle~start_engine,
      stop_engine   FOR if_vehicle~stop_engine,
      get_fuel_level FOR if_vehicle~get_fuel_level.
    
    METHODS: constructor
      IMPORTING iv_model TYPE string.

  PRIVATE SECTION.
    DATA: mv_model    TYPE string,
          mv_fuel     TYPE i VALUE 100,
          mv_engine_on TYPE abap_bool VALUE abap_false.
ENDCLASS.

CLASS lcl_car IMPLEMENTATION.
  METHOD constructor.
    mv_model = iv_model.
  ENDMETHOD.

  METHOD if_vehicle~start_engine.
    IF mv_engine_on = abap_false.
      mv_engine_on = abap_true.
      rv_status = |{ mv_model } car engine started: VROOM!|.
    ELSE.
      rv_status = |{ mv_model } car engine already running|.
    ENDIF.
  ENDMETHOD.

  METHOD if_vehicle~stop_engine.
    IF mv_engine_on = abap_true.
      mv_engine_on = abap_false.
      rv_status = |{ mv_model } car engine stopped|.
    ELSE.
      rv_status = |{ mv_model } car engine already stopped|.
    ENDIF.
  ENDMETHOD.

  METHOD if_vehicle~get_fuel_level.
    rv_fuel = mv_fuel.
    " Simulate fuel consumption when engine is running
    IF mv_engine_on = abap_true AND mv_fuel > 0.
      mv_fuel = mv_fuel - 5.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_MOTORCYCLE
*----------------------------------------------------------------------*
CLASS lcl_motorcycle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_vehicle.
    ALIASES:
      start_engine  FOR if_vehicle~start_engine,
      stop_engine   FOR if_vehicle~stop_engine,
      get_fuel_level FOR if_vehicle~get_fuel_level.
    
    METHODS: constructor
      IMPORTING iv_brand TYPE string.

  PRIVATE SECTION.
    DATA: mv_brand    TYPE string,
          mv_fuel     TYPE i VALUE 50,
          mv_engine_on TYPE abap_bool VALUE abap_false.
ENDCLASS.

CLASS lcl_motorcycle IMPLEMENTATION.
  METHOD constructor.
    mv_brand = iv_brand.
  ENDMETHOD.

  METHOD if_vehicle~start_engine.
    IF mv_engine_on = abap_false.
      mv_engine_on = abap_true.
      rv_status = |{ mv_brand } motorcycle engine started: BRUM!|.
    ELSE.
      rv_status = |{ mv_brand } motorcycle engine already running|.
    ENDIF.
  ENDMETHOD.

  METHOD if_vehicle~stop_engine.
    IF mv_engine_on = abap_true.
      mv_engine_on = abap_false.
      rv_status = |{ mv_brand } motorcycle engine stopped|.
    ELSE.
      rv_status = |{ mv_brand } motorcycle engine already stopped|.
    ENDIF.
  ENDMETHOD.

  METHOD if_vehicle~get_fuel_level.
    rv_fuel = mv_fuel.
    " Simulate fuel consumption when engine is running
    IF mv_engine_on = abap_true AND mv_fuel > 0.
      mv_fuel = mv_fuel - 2.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_car TYPE REF TO if_vehicle,
        lo_bike TYPE REF TO if_vehicle,
        lv_status TYPE string,
        lv_fuel TYPE i.

  CREATE OBJECT lo_car TYPE lcl_car
    EXPORTING iv_model = 'Toyota Camry'.
  
  CREATE OBJECT lo_bike TYPE lcl_motorcycle
    EXPORTING iv_brand = 'Harley Davidson'.

  WRITE: / '=== CAR OPERATIONS ==='.
  lv_status = lo_car->start_engine( ).
  WRITE: / lv_status.
  
  lv_fuel = lo_car->get_fuel_level( ).
  WRITE: / 'Car fuel level:', lv_fuel.
  
  lv_status = lo_car->stop_engine( ).
  WRITE: / lv_status.
  SKIP.

  WRITE: / '=== MOTORCYCLE OPERATIONS ==='.
  lv_status = lo_bike->start_engine( ).
  WRITE: / lv_status.
  
  lv_fuel = lo_bike->get_fuel_level( ).
  WRITE: / 'Motorcycle fuel level:', lv_fuel.
  
  lv_status = lo_bike->stop_engine( ).
  WRITE: / lv_status.
```

---

#### **Exercise 2: Multiple Interfaces**
**Problem Statement:**  
Create a system for documents that can be printed, saved, and emailed. Define three interfaces: `IF_PRINTABLE`, `IF_SAVEABLE`, and `IF_EMAILABLE`. Create a class `LCL_DOCUMENT` that implements all three interfaces. Each interface should have at least 2 methods.

**Solution:**
```abap
*&---------------------------------------------------------------------*
*& Exercise 2 Solution: Multiple Interfaces
*&---------------------------------------------------------------------*
REPORT z_exercise_2_solution.

*----------------------------------------------------------------------*
* INTERFACE 1: IF_PRINTABLE
*----------------------------------------------------------------------*
INTERFACE if_printable.
  CONSTANTS:
    cv_page_size_a4 TYPE c LENGTH 2 VALUE 'A4',
    cv_page_size_legal TYPE c LENGTH 6 VALUE 'LEGAL'.
  
  METHODS:
    print IMPORTING iv_copies TYPE i DEFAULT 1
          RETURNING VALUE(rv_success) TYPE abap_bool,
    get_print_preview RETURNING VALUE(rv_preview) TYPE string,
    set_page_size IMPORTING iv_size TYPE c.
ENDINTERFACE.

*----------------------------------------------------------------------*
* INTERFACE 2: IF_SAVEABLE
*----------------------------------------------------------------------*
INTERFACE if_saveable.
  CONSTANTS:
    cv_format_pdf TYPE c LENGTH 3 VALUE 'PDF',
    cv_format_doc TYPE c LENGTH 3 VALUE 'DOC',
    cv_format_txt TYPE c LENGTH 3 VALUE 'TXT'.
  
  METHODS:
    save_to_file IMPORTING iv_filename TYPE string
                           iv_format   TYPE c DEFAULT cv_format_pdf
                 RETURNING VALUE(rv_success) TYPE abap_bool,
    load_from_file IMPORTING iv_filename TYPE string
                   RETURNING VALUE(rv_success) TYPE abap_bool,
    get_file_size RETURNING VALUE(rv_size_kb) TYPE i.
ENDINTERFACE.

*----------------------------------------------------------------------*
* INTERFACE 3: IF_EMAILABLE
*----------------------------------------------------------------------*
INTERFACE if_emailable.
  METHODS:
    attach_to_email IMPORTING iv_recipient TYPE string
                              iv_subject   TYPE string
                    RETURNING VALUE(rv_success) TYPE abap_bool,
    compress_for_email,
    get_attachment_size RETURNING VALUE(rv_size_kb) TYPE i.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: LCL_DOCUMENT (Implements all three interfaces)
*----------------------------------------------------------------------*
CLASS lcl_document DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      if_printable,
      if_saveable,
      if_emailable.
    
    " Aliases for cleaner access
    ALIASES:
      print        FOR if_printable~print,
      get_preview  FOR if_printable~get_print_preview,
      save_to_file FOR if_saveable~save_to_file,
      attach_email FOR if_emailable~attach_to_email.
    
    METHODS:
      constructor
        IMPORTING
          iv_title   TYPE string
          iv_content TYPE string,
      update_content
        IMPORTING
          iv_new_content TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_title      TYPE string,
      mv_content    TYPE string,
      mv_page_size  TYPE c LENGTH 6 VALUE 'A4',
      mv_file_size  TYPE i VALUE 1024, " in KB
      mv_last_saved TYPE timestamp.
ENDCLASS.

CLASS lcl_document IMPLEMENTATION.
  METHOD constructor.
    mv_title = iv_title.
    mv_content = iv_content.
    GET TIME STAMP FIELD mv_last_saved.
  ENDMETHOD.

  METHOD update_content.
    mv_content = iv_new_content.
    " Increase file size when content is updated
    mv_file_size = mv_file_size + 50.
  ENDMETHOD.

  "=====================================================================
  " IF_PRINTABLE Implementation
  "=====================================================================
  METHOD if_printable~print.
    IF iv_copies <= 0.
      rv_success = abap_false.
      WRITE: / 'Error: Number of copies must be positive'.
      RETURN.
    ENDIF.
    
    DO iv_copies TIMES.
      WRITE: / 'Printing document...'.
      WRITE: / 'Title:', mv_title.
      WRITE: / 'Page Size:', mv_page_size.
      WRITE: / 'Content:', mv_content(50), '...'.
      WRITE: / '--- End of Document ---'.
      SKIP.
    ENDDO.
    
    rv_success = abap_true.
    WRITE: / iv_copies, 'copies printed successfully'.
  ENDMETHOD.

  METHOD if_printable~get_print_preview.
    rv_preview = |Document: { mv_title } | &
                 |(Page: { mv_page_size }) | &
                 |Content: { mv_content(30) }...|.
  ENDMETHOD.

  METHOD if_printable~set_page_size.
    mv_page_size = iv_size.
    WRITE: / 'Page size set to:', iv_size.
  ENDMETHOD.

  "=====================================================================
  " IF_SAVEABLE Implementation
  "=====================================================================
  METHOD if_saveable~save_to_file.
    GET TIME STAMP FIELD mv_last_saved.
    rv_success = abap_true.
    WRITE: / 'Document saved as:', iv_filename.
    WRITE: / 'Format:', iv_format.
    WRITE: / 'Time:', mv_last_saved.
  ENDMETHOD.

  METHOD if_saveable~load_from_file.
    " Simulate loading content from file
    IF iv_filename IS NOT INITIAL.
      rv_success = abap_true.
      WRITE: / 'Document loaded from:', iv_filename.
    ELSE.
      rv_success = abap_false.
      WRITE: / 'Error: Filename is empty'.
    ENDIF.
  ENDMETHOD.

  METHOD if_saveable~get_file_size.
    rv_size_kb = mv_file_size.
  ENDMETHOD.

  "=====================================================================
  " IF_EMAILABLE Implementation
  "=====================================================================
  METHOD if_emailable~attach_to_email.
    rv_success = abap_true.
    WRITE: / 'Document attached to email:'.
    WRITE: / 'To:', iv_recipient.
    WRITE: / 'Subject:', iv_subject.
    WRITE: / 'Attachment:', mv_title.
  ENDMETHOD.

  METHOD if_emailable~compress_for_email.
    " Simulate compression - reduce file size by 50%
    mv_file_size = mv_file_size / 2.
    WRITE: / 'Document compressed for email. New size:', mv_file_size, 'KB'.
  ENDMETHOD.

  METHOD if_emailable~get_attachment_size.
    rv_size_kb = mv_file_size.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_doc TYPE REF TO lcl_document.

  CREATE OBJECT lo_doc
    EXPORTING
      iv_title   = 'Project Proposal'
      iv_content = 'This document outlines the proposal for the new project...'.

  WRITE: / '=== DOCUMENT MANAGEMENT SYSTEM ==='.
  SKIP.

  WRITE: / '1. PRINTING OPERATIONS:'.
  lo_doc->if_printable~set_page_size( if_printable=>cv_page_size_a4 ).
  lo_doc->print( iv_copies = 2 ).
  SKIP.

  WRITE: / '2. SAVING OPERATIONS:'.
  lo_doc->save_to_file(
    iv_filename = 'project_proposal.pdf'
    iv_format   = if_saveable=>cv_format_pdf
  ).
  WRITE: / 'File size:', lo_doc->if_saveable~get_file_size( ), 'KB'.
  SKIP.

  WRITE: / '3. EMAIL OPERATIONS:'.
  lo_doc->if_emailable~compress_for_email( ).
  lo_doc->attach_email(
    iv_recipient = 'manager@company.com'
    iv_subject   = 'Project Proposal Document'
  ).
```

---

#### **Exercise 3: Interface with Events**
**Problem Statement:**  
Create an interface `IF_TEMPERATURE_SENSOR` with an event `temperature_changed` and methods to read temperature and set thresholds. Create a class `LCL_HEATING_SYSTEM` that implements this interface and raises events when temperature changes. Create another class `LCL_TEMPERATURE_MONITOR` that handles the events.

**Solution:**
```abap
*&---------------------------------------------------------------------*
*& Exercise 3 Solution: Interface with Events
*&---------------------------------------------------------------------*
REPORT z_exercise_3_solution.

*----------------------------------------------------------------------*
* INTERFACE: IF_TEMPERATURE_SENSOR
*----------------------------------------------------------------------*
INTERFACE if_temperature_sensor.
  " Event declaration
  EVENTS:
    temperature_changed
      EXPORTING
        VALUE(ev_old_temp) TYPE i
        VALUE(ev_new_temp) TYPE i,
    temperature_alert
      EXPORTING
        VALUE(ev_message) TYPE string.
  
  " Method declarations
  METHODS:
    read_temperature RETURNING VALUE(rv_temp) TYPE i,
    set_threshold IMPORTING iv_min_temp TYPE i
                            iv_max_temp TYPE i,
    get_status RETURNING VALUE(rv_status) TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: LCL_HEATING_SYSTEM (Implements interface and raises events)
*----------------------------------------------------------------------*
CLASS lcl_heating_system DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_temperature_sensor.
    
    ALIASES:
      read_temperature FOR if_temperature_sensor~read_temperature,
      set_threshold    FOR if_temperature_sensor~set_threshold,
      get_status       FOR if_temperature_sensor~get_status.
    
    METHODS:
      constructor,
      update_temperature IMPORTING iv_new_temp TYPE i.

  PRIVATE SECTION.
    DATA:
      mv_current_temp TYPE i VALUE 20,
      mv_min_temp     TYPE i VALUE 18,
      mv_max_temp     TYPE i VALUE 25,
      mv_last_temp    TYPE i VALUE 20.
ENDCLASS.

CLASS lcl_heating_system IMPLEMENTATION.
  METHOD constructor.
    WRITE: / 'Heating system initialized at 20°C'.
  ENDMETHOD.

  METHOD update_temperature.
    mv_last_temp = mv_current_temp.
    mv_current_temp = iv_new_temp.
    
    " Raise temperature_changed event
    RAISE EVENT if_temperature_sensor~temperature_changed
      EXPORTING
        ev_old_temp = mv_last_temp
        ev_new_temp = mv_current_temp.
    
    " Check thresholds and raise alert if needed
    IF mv_current_temp < mv_min_temp.
      RAISE EVENT if_temperature_sensor~temperature_alert
        EXPORTING
          ev_message = |ALERT: Temperature too low! { mv_current_temp }°C < { mv_min_temp }°C|.
    ELSEIF mv_current_temp > mv_max_temp.
      RAISE EVENT if_temperature_sensor~temperature_alert
        EXPORTING
          ev_message = |ALERT: Temperature too high! { mv_current_temp }°C > { mv_max_temp }°C|.
    ENDIF.
    
    WRITE: / 'Temperature updated:', mv_current_temp, '°C'.
  ENDMETHOD.

  METHOD if_temperature_sensor~read_temperature.
    rv_temp = mv_current_temp.
  ENDMETHOD.

  METHOD if_temperature_sensor~set_threshold.
    mv_min_temp = iv_min_temp.
    mv_max_temp = iv_max_temp.
    WRITE: / 'Temperature thresholds set: Min', mv_min_temp, '°C, Max', mv_max_temp, '°C'.
  ENDMETHOD.

  METHOD if_temperature_sensor~get_status.
    IF mv_current_temp BETWEEN mv_min_temp AND mv_max_temp.
      rv_status = 'NORMAL'.
    ELSEIF mv_current_temp < mv_min_temp.
      rv_status = 'TOO COLD'.
    ELSE.
      rv_status = 'TOO HOT'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_TEMPERATURE_MONITOR (Handles events)
*----------------------------------------------------------------------*
CLASS lcl_temperature_monitor DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      on_temperature_changed
        FOR EVENT temperature_changed OF if_temperature_sensor
        IMPORTING ev_old_temp ev_new_temp,
      on_temperature_alert
        FOR EVENT temperature_alert OF if_temperature_sensor
        IMPORTING ev_message,
      connect_to_sensor
        IMPORTING io_sensor TYPE REF TO if_temperature_sensor.

  PRIVATE SECTION.
    DATA:
      mv_monitor_name TYPE string,
      mv_change_count TYPE i.
ENDCLASS.

CLASS lcl_temperature_monitor IMPLEMENTATION.
  METHOD constructor.
    mv_monitor_name = 'Temperature Monitor V1.0'.
    mv_change_count = 0.
    WRITE: / mv_monitor_name, 'created'.
  ENDMETHOD.

  METHOD connect_to_sensor.
    " Register event handlers
    SET HANDLER:
      me->on_temperature_changed FOR io_sensor,
      me->on_temperature_alert FOR io_sensor.
    
    WRITE: / 'Monitor connected to temperature sensor'.
  ENDMETHOD.

  METHOD on_temperature_changed.
    mv_change_count = mv_change_count + 1.
    
    WRITE: / '╔══════════════════════════════════════════════╗'.
    WRITE: / '║ TEMPERATURE CHANGED EVENT                    ║'.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    WRITE: / '║ Old Temperature:', ev_old_temp, '°C'.
    WRITE: / '║ New Temperature:', ev_new_temp, '°C'.
    WRITE: / '║ Change:', ev_new_temp - ev_old_temp, '°C'.
    WRITE: / '║ Total changes monitored:', mv_change_count.
    WRITE: / '╚══════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD on_temperature_alert.
    WRITE: / '╔══════════════════════════════════════════════╗'.
    WRITE: / '║ ⚠️  TEMPERATURE ALERT!                         ║'.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    WRITE: / '║', ev_message.
    WRITE: / '║ Time:', sy-datum, sy-uzeit.
    WRITE: / '║ ACTION REQUIRED: Adjust heating/cooling!     ║'.
    WRITE: / '╚══════════════════════════════════════════════╝'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_EMAIL_NOTIFIER (Another event handler)
*----------------------------------------------------------------------*
CLASS lcl_email_notifier DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_temperature_alert
        FOR EVENT temperature_alert OF if_temperature_sensor
        IMPORTING ev_message,
      subscribe_to_alerts
        IMPORTING io_sensor TYPE REF TO if_temperature_sensor.
ENDCLASS.

CLASS lcl_email_notifier IMPLEMENTATION.
  METHOD subscribe_to_alerts.
    SET HANDLER me->on_temperature_alert FOR io_sensor.
    WRITE: / 'Email notifier subscribed to temperature alerts'.
  ENDMETHOD.

  METHOD on_temperature_alert.
    WRITE: / '📧 EMAIL SENT: Temperature Alert -', ev_message.
    WRITE: / '   Sent to: facility-manager@company.com'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_heating TYPE REF TO lcl_heating_system,
        lo_monitor TYPE REF TO lcl_temperature_monitor,
        lo_notifier TYPE REF TO lcl_email_notifier,
        lo_sensor TYPE REF TO if_temperature_sensor.

  " Create objects
  CREATE OBJECT lo_heating.
  CREATE OBJECT lo_monitor.
  CREATE OBJECT lo_notifier.

  " Get interface reference
  lo_sensor = lo_heating.

  WRITE: / '=== TEMPERATURE MONITORING SYSTEM ==='.
  SKIP.

  " Set up monitoring
  lo_monitor->connect_to_sensor( lo_sensor ).
  lo_notifier->subscribe_to_alerts( lo_sensor ).
  SKIP.

  " Set temperature thresholds
  lo_heating->set_threshold( iv_min_temp = 18 iv_max_temp = 24 ).
  SKIP.

  " Simulate temperature changes
  WRITE: / '=== SIMULATING TEMPERATURE CHANGES ==='.
  SKIP.

  " Normal change
  WRITE: / '1. Normal temperature increase:'.
  lo_heating->update_temperature( 22 ).
  SKIP.

  " Too cold
  WRITE: / '2. Temperature dropping too low:'.
  lo_heating->update_temperature( 16 ).
  SKIP.

  " Too hot
  WRITE: / '3. Temperature rising too high:'.
  lo_heating->update_temperature( 26 ).
  SKIP.

  " Check status
  WRITE: / 'Current system status:', lo_heating->get_status( ).
```

---

#### **Exercise 4: Compound Interfaces**
**Problem Statement:**  
Create a system for different types of employees. Create base interfaces `IF_IDENTIFIABLE`, `IF_SALARY_CALCULATOR`, and `IF_REPORTABLE`. Then create compound interfaces `IF_MANAGER` and `IF_DEVELOPER` that combine these base interfaces differently. Implement classes for each compound interface.

**Solution:**
```abap
*&---------------------------------------------------------------------*
*& Exercise 4 Solution: Compound Interfaces
*&---------------------------------------------------------------------*
REPORT z_exercise_4_solution.

*----------------------------------------------------------------------*
* BASE INTERFACES
*----------------------------------------------------------------------*
" Interface for identifiable objects
INTERFACE if_identifiable.
  METHODS:
    get_id RETURNING VALUE(rv_id) TYPE string,
    get_name RETURNING VALUE(rv_name) TYPE string.
ENDINTERFACE.

" Interface for salary calculation
INTERFACE if_salary_calculator.
  TYPES:
    ty_salary_components TYPE TABLE OF string WITH DEFAULT KEY.
  
  METHODS:
    calculate_salary RETURNING VALUE(rv_salary) TYPE p,
    get_salary_components RETURNING VALUE(rt_components) TYPE ty_salary_components.
ENDINTERFACE.

" Interface for reporting
INTERFACE if_reportable.
  METHODS:
    generate_report RETURNING VALUE(rv_report) TYPE string,
    submit_report IMPORTING iv_recipient TYPE string
                  RETURNING VALUE(rv_success) TYPE abap_bool.
ENDINTERFACE.

*----------------------------------------------------------------------*
* COMPOUND INTERFACE 1: IF_MANAGER
*----------------------------------------------------------------------*
INTERFACE if_manager.
  " Include base interfaces
  INTERFACES:
    if_identifiable,
    if_salary_calculator,
    if_reportable.
  
  " Additional manager-specific methods
  METHODS:
    manage_team IMPORTING iv_team_size TYPE i,
    conduct_meeting IMPORTING iv_topic TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* COMPOUND INTERFACE 2: IF_DEVELOPER
*----------------------------------------------------------------------*
INTERFACE if_developer.
  " Include base interfaces (different combination)
  INTERFACES:
    if_identifiable,
    if_salary_calculator.
  
  " Developer-specific methods
  METHODS:
    write_code IMPORTING iv_language TYPE string
                         iv_task     TYPE string,
    debug_code IMPORTING iv_module TYPE string,
    get_skills RETURNING VALUE(rt_skills) TYPE string_table.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: LCL_MANAGER (Implements IF_MANAGER)
*----------------------------------------------------------------------*
CLASS lcl_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_manager.
    
    " Aliases for cleaner access
    ALIASES:
      get_id         FOR if_manager~if_identifiable~get_id,
      get_name       FOR if_manager~if_identifiable~get_name,
      calculate_salary FOR if_manager~if_salary_calculator~calculate_salary,
      generate_report FOR if_manager~if_reportable~generate_report,
      manage_team    FOR if_manager~manage_team.
    
    METHODS:
      constructor
        IMPORTING
          iv_id       TYPE string
          iv_name     TYPE string
          iv_base_salary TYPE p.

  PRIVATE SECTION.
    DATA:
      mv_id         TYPE string,
      mv_name       TYPE string,
      mv_base_salary TYPE p DECIMALS 2,
      mv_team_size  TYPE i.
ENDCLASS.

CLASS lcl_manager IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
    mv_name = iv_name.
    mv_base_salary = iv_base_salary.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_IDENTIFIABLE Implementation
  "-------------------------------------------------------------------
  METHOD if_manager~if_identifiable~get_id.
    rv_id = mv_id.
  ENDMETHOD.

  METHOD if_manager~if_identifiable~get_name.
    rv_name = |Manager: { mv_name }|.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_SALARY_CALCULATOR Implementation
  "-------------------------------------------------------------------
  METHOD if_manager~if_salary_calculator~calculate_salary.
    " Manager: Base salary + 30% bonus + 10000 per team member
    DATA(lv_bonus) = mv_base_salary * '0.30'.
    DATA(lv_team_bonus) = mv_team_size * 10000.
    rv_salary = mv_base_salary + lv_bonus + lv_team_bonus.
  ENDMETHOD.

  METHOD if_manager~if_salary_calculator~get_salary_components.
    APPEND 'Base Salary' TO rt_components.
    APPEND '30% Performance Bonus' TO rt_components.
    APPEND 'Team Management Allowance' TO rt_components.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_REPORTABLE Implementation
  "-------------------------------------------------------------------
  METHOD if_manager~if_reportable~generate_report.
    DATA(lv_salary) = if_manager~if_salary_calculator~calculate_salary( ).
    rv_report = |Manager Report - { mv_name }| &
                |\nTeam Size: { mv_team_size }| &
                |\nBase Salary: { mv_base_salary }| &
                |\nTotal Salary: { lv_salary }| &
                |\nGenerated on: { sy-datum }|.
  ENDMETHOD.

  METHOD if_manager~if_reportable~submit_report.
    rv_success = abap_true.
    WRITE: / 'Manager report submitted to:', iv_recipient.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_MANAGER Specific Methods
  "-------------------------------------------------------------------
  METHOD if_manager~manage_team.
    mv_team_size = iv_team_size.
    WRITE: / 'Managing team of', iv_team_size, 'members'.
  ENDMETHOD.

  METHOD if_manager~conduct_meeting.
    WRITE: / 'Conducting meeting on topic:', iv_topic.
    WRITE: / 'Attendees:', mv_team_size, 'team members'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_DEVELOPER (Implements IF_DEVELOPER)
*----------------------------------------------------------------------*
CLASS lcl_developer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_developer.
    
    " Aliases
    ALIASES:
      get_id         FOR if_developer~if_identifiable~get_id,
      get_name       FOR if_developer~if_identifiable~get_name,
      calculate_salary FOR if_developer~if_salary_calculator~calculate_salary,
      write_code     FOR if_developer~write_code.
    
    METHODS:
      constructor
        IMPORTING
          iv_id       TYPE string
          iv_name     TYPE string
          iv_base_salary TYPE p
          iv_language TYPE string,
      add_skill IMPORTING iv_skill TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_id         TYPE string,
      mv_name       TYPE string,
      mv_base_salary TYPE p DECIMALS 2,
      mv_language   TYPE string,
      mt_skills     TYPE string_table.
ENDCLASS.

CLASS lcl_developer IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
    mv_name = iv_name.
    mv_base_salary = iv_base_salary.
    mv_language = iv_language.
    add_skill( iv_language ).
  ENDMETHOD.

  METHOD add_skill.
    APPEND iv_skill TO mt_skills.
    WRITE: / 'Skill added:', iv_skill.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_IDENTIFIABLE Implementation
  "-------------------------------------------------------------------
  METHOD if_developer~if_identifiable~get_id.
    rv_id = mv_id.
  ENDMETHOD.

  METHOD if_developer~if_identifiable~get_name.
    rv_name = |Developer: { mv_name } ({ mv_language })|.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_SALARY_CALCULATOR Implementation
  "-------------------------------------------------------------------
  METHOD if_developer~if_salary_calculator~calculate_salary.
    " Developer: Base salary + 20% bonus + 5000 per skill
    DATA(lv_bonus) = mv_base_salary * '0.20'.
    DATA(lv_skill_bonus) = lines( mt_skills ) * 5000.
    rv_salary = mv_base_salary + lv_bonus + lv_skill_bonus.
  ENDMETHOD.

  METHOD if_developer~if_salary_calculator~get_salary_components.
    APPEND 'Base Salary' TO rt_components.
    APPEND '20% Performance Bonus' TO rt_components.
    APPEND 'Skill-based Allowance' TO rt_components.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_DEVELOPER Specific Methods
  "-------------------------------------------------------------------
  METHOD if_developer~write_code.
    WRITE: / 'Writing code in', mv_language, 'for task:', iv_task.
    WRITE: / 'Lines of code written: 100+'.
  ENDMETHOD.

  METHOD if_developer~debug_code.
    WRITE: / 'Debugging module:', iv_module.
    WRITE: / 'Bugs found: 3, Fixed: 3'.
  ENDMETHOD.

  METHOD if_developer~get_skills.
    rt_skills = mt_skills.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: PAYROLL SYSTEM (Works with both managers and developers)
*----------------------------------------------------------------------*
CLASS lcl_payroll_system DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      process_payroll
        IMPORTING it_employees TYPE TABLE,
      print_employee_directory
        IMPORTING it_employees TYPE TABLE,
      calculate_total_payroll
        IMPORTING it_employees TYPE TABLE
        RETURNING VALUE(rv_total) TYPE p.
ENDCLASS.

CLASS lcl_payroll_system IMPLEMENTATION.
  METHOD process_payroll.
    DATA: lo_employee TYPE REF TO if_identifiable,
          lo_salary   TYPE REF TO if_salary_calculator,
          lv_salary   TYPE p.
    
    WRITE: / '╔══════════════════════════════════════════════╗'.
    WRITE: / '║           PAYROLL PROCESSING                 ║'.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    
    LOOP AT it_employees INTO lo_employee.
      lo_salary ?= lo_employee.  " Cast to salary calculator
      
      IF lo_salary IS BOUND.
        lv_salary = lo_salary->calculate_salary( ).
        WRITE: / '║ Employee:', lo_employee->get_name( ).
        WRITE: / '║ ID:', lo_employee->get_id( ).
        WRITE: / '║ Salary: $', lv_salary.
        WRITE: / '╠══════════════════════════════════════════════╣'.
      ENDIF.
    ENDLOOP.
    
    WRITE: / '║           END OF PAYROLL RUN                ║'.
    WRITE: / '╚══════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD print_employee_directory.
    DATA: lo_employee TYPE REF TO if_identifiable.
    
    WRITE: / '=== EMPLOYEE DIRECTORY ==='.
    LOOP AT it_employees INTO lo_employee.
      WRITE: / '-', lo_employee->get_name( ), '(ID:', lo_employee->get_id( ), ')'.
    ENDLOOP.
    WRITE: / 'Total Employees:', lines( it_employees ).
  ENDMETHOD.

  METHOD calculate_total_payroll.
    DATA: lo_salary TYPE REF TO if_salary_calculator.
    
    LOOP AT it_employees INTO lo_salary.
      rv_total = rv_total + lo_salary->calculate_salary( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_manager1 TYPE REF TO lcl_manager,
        lo_manager2 TYPE REF TO lcl_manager,
        lo_dev1     TYPE REF TO lcl_developer,
        lo_dev2     TYPE REF TO lcl_developer.
  
  DATA: lt_employees TYPE TABLE OF REF TO if_identifiable.
  DATA: lo_employee TYPE REF TO if_identifiable.

  " Create employees
  CREATE OBJECT lo_manager1
    EXPORTING
      iv_id         = 'M001'
      iv_name       = 'Alice Johnson'
      iv_base_salary = '80000'.
  
  CREATE OBJECT lo_manager2
    EXPORTING
      iv_id         = 'M002'
      iv_name       = 'Bob Smith'
      iv_base_salary = '75000'.
  
  CREATE OBJECT lo_dev1
    EXPORTING
      iv_id         = 'D001'
      iv_name       = 'Charlie Brown'
      iv_base_salary = '60000'
      iv_language   = 'ABAP'.
  
  CREATE OBJECT lo_dev2
    EXPORTING
      iv_id         = 'D002'
      iv_name       = 'Diana Prince'
      iv_base_salary = '65000'
      iv_language   = 'Java'.

  " Add employees to list
  lo_employee = lo_manager1.
  APPEND lo_employee TO lt_employees.
  
  lo_employee = lo_manager2.
  APPEND lo_employee TO lt_employees.
  
  lo_employee = lo_dev1.
  APPEND lo_employee TO lt_employees.
  
  lo_employee = lo_dev2.
  APPEND lo_employee TO lt_employees.

  " Set up managers
  lo_manager1->manage_team( 5 ).
  lo_manager2->manage_team( 3 ).
  
  " Add skills to developers
  lo_dev1->if_developer~add_skill( 'SAP FIORI' ).
  lo_dev1->if_developer~add_skill( 'SAPUI5' ).
  lo_dev2->if_developer~add_skill( 'Spring Boot' ).
  lo_dev2->if_developer~add_skill( 'Microservices' ).
  SKIP.

  " Use payroll system
  WRITE: / '=== PAYROLL PROCESSING ==='.
  lcl_payroll_system=>print_employee_directory( lt_employees ).
  SKIP.
  
  lcl_payroll_system=>process_payroll( lt_employees ).
  SKIP.
  
  DATA(lv_total) = lcl_payroll_system=>calculate_total_payroll( lt_employees ).
  WRITE: / 'TOTAL COMPANY PAYROLL: $', lv_total.
  SKIP.

  " Demonstrate specific functionality
  WRITE: / '=== SPECIFIC OPERATIONS ==='.
  lo_manager1->conduct_meeting( 'Q4 Planning' ).
  SKIP.
  
  lo_dev1->write_code( iv_language = 'ABAP' iv_task = 'Implement BAPI' ).
  lo_dev1->debug_code( 'FI Module' ).
  SKIP.
  
  " Generate manager report
  DATA(lv_report) = lo_manager2->generate_report( ).
  WRITE: / 'Manager Report:'.
  WRITE: / lv_report.
```

---

#### **Exercise 5: Real-World Banking System**
**Problem Statement:**  
Create a banking system with interfaces for different account types. Create interfaces `IF_BANK_ACCOUNT`, `IF_INTEREST_CALCULATOR`, and `IF_TRANSACTION_HISTORY`. Implement classes for `SAVINGS_ACCOUNT`, `CURRENT_ACCOUNT`, and `FIXED_DEPOSIT` that implement these interfaces differently.

**Solution:**
```abap
*&---------------------------------------------------------------------*
*& Exercise 5 Solution: Banking System
*&---------------------------------------------------------------------*
REPORT z_exercise_5_solution.

*----------------------------------------------------------------------*
* BASE INTERFACES
*----------------------------------------------------------------------*
INTERFACE if_bank_account.
  TYPES:
    BEGIN OF ty_account_details,
      account_no   TYPE numc10,
      holder_name  TYPE string,
      balance      TYPE p DECIMALS 2,
      opened_date  TYPE datum,
    END OF ty_account_details.
  
  METHODS:
    get_account_details RETURNING VALUE(rs_details) TYPE ty_account_details,
    deposit  IMPORTING iv_amount TYPE p
             RETURNING VALUE(rv_new_balance) TYPE p,
    withdraw IMPORTING iv_amount TYPE p
             RETURNING VALUE(rv_new_balance) TYPE p,
    get_balance RETURNING VALUE(rv_balance) TYPE p.
ENDINTERFACE.

INTERFACE if_interest_calculator.
  CONSTANTS:
    cv_interest_rate_savings TYPE p DECIMALS 2 VALUE '4.50',
    cv_interest_rate_current TYPE p DECIMALS 2 VALUE '0.50',
    cv_interest_rate_fixed   TYPE p DECIMALS 2 VALUE '6.50'.
  
  METHODS:
    calculate_interest RETURNING VALUE(rv_interest) TYPE p,
    apply_interest,
    get_interest_rate RETURNING VALUE(rv_rate) TYPE p.
ENDINTERFACE.

INTERFACE if_transaction_history.
  TYPES:
    BEGIN OF ty_transaction,
      date        TYPE datum,
      time        TYPE uzeit,
      type        TYPE c LENGTH 1,  " D=Deposit, W=Withdrawal, I=Interest
      amount      TYPE p DECIMALS 2,
      balance     TYPE p DECIMALS 2,
      description TYPE string,
    END OF ty_transaction,
    ty_transactions TYPE STANDARD TABLE OF ty_transaction WITH DEFAULT KEY.
  
  METHODS:
    add_transaction IMPORTING is_transaction TYPE ty_transaction,
    get_last_n_transactions IMPORTING iv_count TYPE i
                            RETURNING VALUE(rt_transactions) TYPE ty_transactions,
    get_statement IMPORTING iv_from_date TYPE datum
                            iv_to_date   TYPE datum
                  RETURNING VALUE(rv_statement) TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* COMPOUND INTERFACE: IF_ACCOUNT (Combines all interfaces)
*----------------------------------------------------------------------*
INTERFACE if_account.
  INTERFACES:
    if_bank_account,
    if_interest_calculator,
    if_transaction_history.
  
  " Additional account methods
  METHODS:
    close_account RETURNING VALUE(rv_success) TYPE abap_bool,
    get_account_type RETURNING VALUE(rv_type) TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* ABSTRACT BASE CLASS (Common functionality)
*----------------------------------------------------------------------*
CLASS lcl_account_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_account_no  TYPE numc10
          iv_holder_name TYPE string
          iv_initial_deposit TYPE p OPTIONAL.

  PROTECTED SECTION.
    DATA:
      ms_details       TYPE if_bank_account=>ty_account_details,
      mt_transactions  TYPE if_transaction_history=>ty_transactions,
      mv_interest_rate TYPE p DECIMALS 2.
    
    METHODS:
      add_transaction_record
        IMPORTING
          iv_type        TYPE c
          iv_amount      TYPE p
          iv_description TYPE string.
ENDCLASS.

CLASS lcl_account_base IMPLEMENTATION.
  METHOD constructor.
    ms_details-account_no  = iv_account_no.
    ms_details-holder_name = iv_holder_name.
    ms_details-opened_date = sy-datum.
    
    IF iv_initial_deposit IS SUPPLIED.
      ms_details-balance = iv_initial_deposit.
      add_transaction_record(
        iv_type   = 'D'
        iv_amount = iv_initial_deposit
        iv_description = 'Initial Deposit'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD add_transaction_record.
    DATA: ls_transaction TYPE if_transaction_history=>ty_transaction.
    
    ls_transaction-date    = sy-datum.
    ls_transaction-time    = sy-uzeit.
    ls_transaction-type    = iv_type.
    ls_transaction-amount  = iv_amount.
    ls_transaction-balance = ms_details-balance.
    ls_transaction-description = iv_description.
    
    APPEND ls_transaction TO mt_transactions.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_SAVINGS_ACCOUNT
*----------------------------------------------------------------------*
CLASS lcl_savings_account DEFINITION
  INHERITING FROM lcl_account_base.
  PUBLIC SECTION.
    INTERFACES: if_account.
    
    ALIASES:
      deposit  FOR if_account~if_bank_account~deposit,
      withdraw FOR if_account~if_bank_account~withdraw,
      get_balance FOR if_account~if_bank_account~get_balance.
    
    METHODS:
      constructor
        IMPORTING
          iv_account_no  TYPE numc10
          iv_holder_name TYPE string
          iv_initial_deposit TYPE p OPTIONAL.
ENDCLASS.

CLASS lcl_savings_account IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_account_no      = iv_account_no
      iv_holder_name     = iv_holder_name
      iv_initial_deposit = iv_initial_deposit
    ).
    mv_interest_rate = if_interest_calculator=>cv_interest_rate_savings.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_BANK_ACCOUNT Implementation
  "-------------------------------------------------------------------
  METHOD if_account~if_bank_account~get_account_details.
    rs_details = ms_details.
  ENDMETHOD.

  METHOD if_account~if_bank_account~deposit.
    IF iv_amount <= 0.
      WRITE: / 'Error: Deposit amount must be positive'.
      rv_new_balance = ms_details-balance.
      RETURN.
    ENDIF.
    
    ms_details-balance = ms_details-balance + iv_amount.
    rv_new_balance = ms_details-balance.
    
    add_transaction_record(
      iv_type   = 'D'
      iv_amount = iv_amount
      iv_description = 'Cash Deposit'
    ).
    
    WRITE: / 'Deposited:', iv_amount, 'New Balance:', rv_new_balance.
  ENDMETHOD.

  METHOD if_account~if_bank_account~withdraw.
    " Savings account: Minimum balance requirement
    DATA(lv_min_balance) = 1000.
    
    IF iv_amount <= 0.
      WRITE: / 'Error: Withdrawal amount must be positive'.
      rv_new_balance = ms_details-balance.
      RETURN.
    ENDIF.
    
    IF ms_details-balance - iv_amount < lv_min_balance.
      WRITE: / 'Error: Insufficient funds. Minimum balance:', lv_min_balance.
      rv_new_balance = ms_details-balance.
      RETURN.
    ENDIF.
    
    ms_details-balance = ms_details-balance - iv_amount.
    rv_new_balance = ms_details-balance.
    
    add_transaction_record(
      iv_type   = 'W'
      iv_amount = iv_amount
      iv_description = 'Cash Withdrawal'
    ).
    
    WRITE: / 'Withdrawn:', iv_amount, 'New Balance:', rv_new_balance.
  ENDMETHOD.

  METHOD if_account~if_bank_account~get_balance.
    rv_balance = ms_details-balance.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_INTEREST_CALCULATOR Implementation
  "-------------------------------------------------------------------
  METHOD if_account~if_interest_calculator~calculate_interest.
    " Monthly interest calculation
    rv_interest = ( ms_details-balance * mv_interest_rate ) / ( 100 * 12 ).
  ENDMETHOD.

  METHOD if_account~if_interest_calculator~apply_interest.
    DATA(lv_interest) = if_account~if_interest_calculator~calculate_interest( ).
    ms_details-balance = ms_details-balance + lv_interest.
    
    add_transaction_record(
      iv_type   = 'I'
      iv_amount = lv_interest
      iv_description = 'Monthly Interest'
    ).
    
    WRITE: / 'Interest applied:', lv_interest.
    WRITE: / 'New balance after interest:', ms_details-balance.
  ENDMETHOD.

  METHOD if_account~if_interest_calculator~get_interest_rate.
    rv_rate = mv_interest_rate.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_TRANSACTION_HISTORY Implementation
  "-------------------------------------------------------------------
  METHOD if_account~if_transaction_history~add_transaction.
    APPEND is_transaction TO mt_transactions.
  ENDMETHOD.

  METHOD if_account~if_transaction_history~get_last_n_transactions.
    DATA: lv_index TYPE i.
    
    lv_index = lines( mt_transactions ).
    
    DO iv_count TIMES.
      IF lv_index > 0.
        READ TABLE mt_transactions INTO DATA(ls_trans) INDEX lv_index.
        APPEND ls_trans TO rt_transactions.
        lv_index = lv_index - 1.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD if_account~if_transaction_history~get_statement.
    DATA: lt_filtered TYPE if_transaction_history=>ty_transactions.
    
    LOOP AT mt_transactions INTO DATA(ls_trans)
         WHERE date BETWEEN iv_from_date AND iv_to_date.
      APPEND ls_trans TO lt_filtered.
    ENDLOOP.
    
    rv_statement = |Statement for { ms_details-holder_name }| &
                   |\nAccount: { ms_details-account_no }| &
                   |\nPeriod: { iv_from_date } to { iv_to_date }| &
                   |\n\nDate       | Type | Amount    | Balance   | Description|.
    
    LOOP AT lt_filtered INTO DATA(ls_stmt).
      rv_statement = rv_statement & 
                   |\n{ ls_stmt-date } | { ls_stmt-type } | { ls_stmt-amount } | { ls_stmt-balance } | { ls_stmt-description }|.
    ENDLOOP.
    
    rv_statement = rv_statement & |\n\nEnding Balance: { ms_details-balance }|.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_ACCOUNT Specific Methods
  "-------------------------------------------------------------------
  METHOD if_account~close_account.
    IF ms_details-balance > 0.
      WRITE: / 'Error: Account has balance. Withdraw funds first.'.
      rv_success = abap_false.
    ELSE.
      WRITE: / 'Savings account closed successfully'.
      rv_success = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD if_account~get_account_type.
    rv_type = 'SAVINGS ACCOUNT'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: LCL_CURRENT_ACCOUNT
*----------------------------------------------------------------------*
CLASS lcl_current_account DEFINITION
  INHERITING FROM lcl_account_base.
  PUBLIC SECTION.
    INTERFACES: if_account.
    
    ALIASES:
      deposit  FOR if_account~if_bank_account~deposit,
      withdraw FOR if_account~if_bank_account~withdraw.
    
    METHODS:
      constructor
        IMPORTING
          iv_account_no  TYPE numc10
          iv_holder_name TYPE string
          iv_overdraft_limit TYPE p OPTIONAL.
ENDCLASS.

CLASS lcl_current_account IMPLEMENTATION.
  METHOD constructor.
    super->constructor(
      iv_account_no  = iv_account_no
      iv_holder_name = iv_holder_name
    ).
    mv_interest_rate = if_interest_calculator=>cv_interest_rate_current.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " IF_BANK_ACCOUNT Implementation (Overridden for current account)
  "-------------------------------------------------------------------
  METHOD if_account~if_bank_account~withdraw.
    " Current account: Allows overdraft
    DATA(lv_overdraft_limit) = 50000.  " Default overdraft limit
    
    IF ms_details-balance - iv_amount >= -lv_overdraft_limit.
      ms_details-balance = ms_details-balance - iv_amount.
      rv_new_balance = ms_details-balance.
      
      add_transaction_record(
        iv_type   = 'W'
        iv_amount = iv_amount
        iv_description = 'Current Account Withdrawal'
      ).
      
      WRITE: / 'Withdrawn:', iv_amount, 'New Balance:', rv_new_balance.
      
      IF ms_details-balance < 0.
        WRITE: / '⚠️  Overdraft used. Balance:', ms_details-balance.
      ENDIF.
    ELSE.
      WRITE: / 'Error: Overdraft limit exceeded. Limit:', lv_overdraft_limit.
      rv_new_balance = ms_details-balance.
    ENDIF.
  ENDMETHOD.

  " Other methods similar to savings account but with different rates/rules
  METHOD if_account~if_interest_calculator~calculate_interest.
    " Current account: Interest only on positive balance
    IF ms_details-balance > 0.
      rv_interest = ( ms_details-balance * mv_interest_rate ) / ( 100 * 12 ).
    ELSE.
      rv_interest = 0.
    ENDIF.
  ENDMETHOD.

  METHOD if_account~get_account_type.
    rv_type = 'CURRENT ACCOUNT'.
  ENDMETHOD.
  
  " Other interface methods would be implemented similarly
  " but omitted for brevity in this example
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: BANKING SYSTEM
*----------------------------------------------------------------------*
CLASS lcl_banking_system DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_account
        IMPORTING
          iv_account_type TYPE string
          iv_account_no   TYPE numc10
          iv_holder_name  TYPE string
        RETURNING
          VALUE(ro_account) TYPE REF TO if_account,
      transfer_funds
        IMPORTING
          io_from_account TYPE REF TO if_account
          io_to_account   TYPE REF TO if_account
          iv_amount       TYPE p
        RETURNING
          VALUE(rv_success) TYPE abap_bool,
      print_account_summary
        IMPORTING
          io_account TYPE REF TO if_account.
ENDCLASS.

CLASS lcl_banking_system IMPLEMENTATION.
  METHOD create_account.
    CASE iv_account_type.
      WHEN 'SAVINGS'.
        CREATE OBJECT ro_account TYPE lcl_savings_account
          EXPORTING
            iv_account_no  = iv_account_no
            iv_holder_name = iv_holder_name
            iv_initial_deposit = 1000.  " Minimum opening balance
        WRITE: / 'Savings account created successfully'.
      
      WHEN 'CURRENT'.
        CREATE OBJECT ro_account TYPE lcl_current_account
          EXPORTING
            iv_account_no      = iv_account_no
            iv_holder_name     = iv_holder_name
            iv_overdraft_limit = 50000.
        WRITE: / 'Current account created successfully'.
      
      WHEN OTHERS.
        WRITE: / 'Error: Unknown account type'.
    ENDCASE.
  ENDMETHOD.

  METHOD transfer_funds.
    DATA: lv_from_balance TYPE p,
          lv_to_balance   TYPE p.
    
    " Withdraw from source account
    lv_from_balance = io_from_account->if_bank_account~withdraw( iv_amount ).
    
    " Deposit to target account
    IF lv_from_balance <> io_from_account->if_bank_account~get_balance( ).
      " Withdrawal failed
      rv_success = abap_false.
      RETURN.
    ENDIF.
    
    lv_to_balance = io_to_account->if_bank_account~deposit( iv_amount ).
    
    rv_success = abap_true.
    WRITE: / 'Funds transfer successful:'.
    WRITE: / 'Amount:', iv_amount.
    WRITE: / 'From account balance:', lv_from_balance.
    WRITE: / 'To account balance:', lv_to_balance.
  ENDMETHOD.

  METHOD print_account_summary.
    DATA: ls_details TYPE if_bank_account=>ty_account_details.
    
    ls_details = io_account->if_bank_account~get_account_details( ).
    
    WRITE: / '╔══════════════════════════════════════════════╗'.
    WRITE: / '║           ACCOUNT SUMMARY                    ║'.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    WRITE: / '║ Account Type:', io_account->get_account_type( ).
    WRITE: / '║ Account No:', ls_details-account_no.
    WRITE: / '║ Holder:', ls_details-holder_name.
    WRITE: / '║ Balance: $', ls_details-balance.
    WRITE: / '║ Opened:', ls_details-opened_date.
    WRITE: / '║ Interest Rate:', io_account->if_interest_calculator~get_interest_rate( ), '%'.
    WRITE: / '╚══════════════════════════════════════════════╝'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_savings_acct TYPE REF TO if_account,
        lo_current_acct TYPE REF TO if_account,
        lv_success      TYPE abap_bool.

  WRITE: / '=== BANKING SYSTEM DEMONSTRATION ==='.
  SKIP.

  " Create accounts
  WRITE: / '1. CREATING ACCOUNTS:'.
  lo_savings_acct = lcl_banking_system=>create_account(
    iv_account_type = 'SAVINGS'
    iv_account_no   = '1000000001'
    iv_holder_name  = 'John Doe'
  ).
  SKIP.

  lo_current_acct = lcl_banking_system=>create_account(
    iv_account_type = 'CURRENT'
    iv_account_no   = '2000000001'
    iv_holder_name  = 'Jane Smith'
  ).
  SKIP 2.

  " Perform transactions
  WRITE: / '2. PERFORMING TRANSACTIONS:'.
  lo_savings_acct->deposit( 5000 ).
  SKIP.

  lo_savings_acct->withdraw( 2000 ).
  SKIP.

  " Try to withdraw more than minimum balance
  lo_savings_acct->withdraw( 3500 ).
  SKIP.

  " Transfer funds
  WRITE: / '3. FUNDS TRANSFER:'.
  lv_success = lcl_banking_system=>transfer_funds(
    io_from_account = lo_savings_acct
    io_to_account   = lo_current_acct
    iv_amount       = 1000
  ).
  SKIP 2.

  " Apply interest
  WRITE: / '4. INTEREST APPLICATION:'.
  lo_savings_acct->if_interest_calculator~apply_interest( ).
  SKIP.

  lo_current_acct->if_interest_calculator~apply_interest( ).
  SKIP 2.

  " Print account summaries
  WRITE: / '5. ACCOUNT SUMMARIES:'.
  lcl_banking_system=>print_account_summary( lo_savings_acct ).
  SKIP.
  
  lcl_banking_system=>print_account_summary( lo_current_acct ).
  SKIP 2.

  " Get transaction history
  WRITE: / '6. TRANSACTION HISTORY:'.
  DATA(lv_statement) = lo_savings_acct->if_transaction_history~get_statement(
    iv_from_date = sy-datum
    iv_to_date   = sy-datum
  ).
  WRITE: / lv_statement.
```

---

### **Complex Real-World Problems**

#### **Problem 1: E-commerce Payment Gateway System**
**Create a payment processing system with multiple payment methods using interfaces.**

**Requirements:**
1. Create interface `IF_PAYMENT_PROCESSOR` with methods `process_payment()`, `validate_payment()`, and `generate_receipt()`.
2. Create interfaces for different payment types: `IF_CREDIT_CARD_PAYMENT`, `IF_UPI_PAYMENT`, `IF_NET_BANKING`.
3. Each payment method should have specific validation rules and processing logic.
4. Create a factory class `CL_PAYMENT_FACTORY` that returns the appropriate payment processor based on payment type.
5. Implement logging and error handling through interfaces.

**Solution Highlights:**
```abap
*&---------------------------------------------------------------------*
*& Complex Problem 1: E-commerce Payment Gateway
*&---------------------------------------------------------------------*

INTERFACE if_payment_processor.
  METHODS:
    process_payment
      IMPORTING
        is_payment_details TYPE any
      RETURNING
        VALUE(rv_status) TYPE string
      RAISING
        cx_payment_error,
    validate_payment
      IMPORTING
        is_payment_details TYPE any
      RETURNING
        VALUE(rv_valid) TYPE abap_bool
      RAISING
        cx_validation_error,
    generate_receipt
      IMPORTING
        is_payment_details TYPE any
      RETURNING
        VALUE(rv_receipt) TYPE string.
ENDINTERFACE.

INTERFACE if_credit_card_payment.
  INTERFACES: if_payment_processor.
  
  TYPES:
    BEGIN OF ty_cc_details,
      card_number    TYPE string,
      expiry_date    TYPE string,
      cvv            TYPE string,
      card_holder    TYPE string,
      amount         TYPE p DECIMALS 2,
      currency       TYPE string,
    END OF ty_cc_details.
  
  METHODS:
    authenticate_3d_secure RETURNING VALUE(rv_success) TYPE abap_bool,
    check_card_network RETURNING VALUE(rv_network) TYPE string.
ENDINTERFACE.

INTERFACE if_upi_payment.
  INTERFACES: if_payment_processor.
  
  TYPES:
    BEGIN OF ty_upi_details,
      upi_id         TYPE string,
      pin            TYPE string,
      amount         TYPE p DECIMALS 2,
      merchant_id    TYPE string,
    END OF ty_upi_details.
  
  METHODS:
    send_upi_request RETURNING VALUE(rv_response) TYPE string,
    verify_upi_status IMPORTING iv_transaction_id TYPE string.
ENDINTERFACE.

CLASS cl_payment_factory DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_payment_processor
        IMPORTING
          iv_payment_type TYPE string
        RETURNING
          VALUE(ro_processor) TYPE REF TO if_payment_processor
        RAISING
          cx_payment_error.
ENDCLASS.

CLASS cl_credit_card_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_credit_card_payment.
    
    METHODS:
      constructor
        IMPORTING
          iv_merchant_key TYPE string.
ENDCLASS.

CLASS cl_upi_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_upi_payment.
    
    METHODS:
      constructor
        IMPORTING
          iv_bank_gateway_url TYPE string.
ENDCLASS.

" Implementation would follow similar pattern as previous exercises
```

---

#### **Problem 2: Inventory Management System with Observer Pattern**
**Create an inventory system where multiple observers can monitor stock changes.**

**Requirements:**
1. Create interface `IF_INVENTORY_OBSERVABLE` with events for stock changes.
2. Create interface `IF_INVENTORY_OBSERVER` for classes that want to monitor inventory.
3. Implement classes for different observers: `CL_EMAIL_NOTIFIER`, `CL_SMS_NOTIFIER`, `CL_DASHBOARD_UPDATER`.
4. Create inventory class `CL_WAREHOUSE_INVENTORY` that implements observable interface.
5. When stock changes below threshold, all observers should be notified.

**Solution Highlights:**
```abap
*&---------------------------------------------------------------------*
*& Complex Problem 2: Inventory Management with Observer Pattern
*&---------------------------------------------------------------------*

INTERFACE if_inventory_observable.
  EVENTS:
    stock_level_changed
      EXPORTING
        VALUE(ev_product_id) TYPE matnr
        VALUE(ev_old_qty)    TYPE menge_d
        VALUE(ev_new_qty)    TYPE menge_d
        VALUE(ev_timestamp)  TYPE timestamp,
    low_stock_alert
      EXPORTING
        VALUE(ev_product_id) TYPE matnr
        VALUE(ev_current_qty) TYPE menge_d
        VALUE(ev_minimum_qty) TYPE menge_d
        VALUE(ev_alert_level) TYPE string,
    stock_replenished
      EXPORTING
        VALUE(ev_product_id) TYPE matnr
        VALUE(ev_replenished_qty) TYPE menge_d
        VALUE(ev_supplier) TYPE string.
ENDINTERFACE.

INTERFACE if_inventory_observer.
  METHODS:
    on_stock_level_changed
      FOR EVENT stock_level_changed OF if_inventory_observable
      IMPORTING ev_product_id ev_old_qty ev_new_qty ev_timestamp,
    on_low_stock_alert
      FOR EVENT low_stock_alert OF if_inventory_observable
      IMPORTING ev_product_id ev_current_qty ev_minimum_qty ev_alert_level,
    on_stock_replenished
      FOR EVENT stock_replenished OF if_inventory_observable
      IMPORTING ev_product_id ev_replenished_qty ev_supplier.
ENDINTERFACE.

CLASS cl_warehouse_inventory DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_inventory_observable.
    
    METHODS:
      constructor,
      update_stock
        IMPORTING
          iv_product_id TYPE matnr
          iv_new_qty    TYPE menge_d
        RAISING
          cx_inventory_error,
      register_observer
        IMPORTING
          io_observer TYPE REF TO if_inventory_observer,
      unregister_observer
        IMPORTING
          io_observer TYPE REF TO if_inventory_observer.
  
  PRIVATE SECTION.
    DATA:
      mt_observers TYPE TABLE OF REF TO if_inventory_observer,
      mt_inventory TYPE SORTED TABLE OF ty_inventory_item
                   WITH UNIQUE KEY product_id.
ENDCLASS.

CLASS cl_email_notifier DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_inventory_observer.
    
    METHODS:
      constructor
        IMPORTING
          iv_email_address TYPE string.
ENDCLASS.

CLASS cl_dashboard_updater DEFINITION.
  PUBLIC SECTION.
    INTERFACES: if_inventory_observer.
    
    METHODS:
      update_dashboard
        IMPORTING
          is_inventory_data TYPE any.
ENDCLASS.
```

---

### **Chapter Summary**

#### **Key Takeaways:**
1. **Interfaces define contracts** - They specify WHAT must be done, not HOW.
2. **Multiple inheritance** - Classes can implement multiple interfaces, solving the single inheritance limitation.
3. **Loose coupling** - Programs should depend on interfaces, not concrete implementations.
4. **Polymorphism** - Interface references can hold any implementing object.
5. **Interface segregation** - Create small, focused interfaces instead of large ones.

#### **When to Use Interfaces:**
- ✅ When unrelated classes need similar behavior
- ✅ When you need to define an API/contract
- ✅ When implementing multiple behaviors
- ✅ When you want to enable dependency injection
- ✅ When you need to support future extensions

#### **Best Practices:**
1. **Name interfaces clearly** - Use descriptive names like `IF_PRINTABLE`, `IF_SAVEABLE`
2. **Keep interfaces focused** - Follow Interface Segregation Principle
3. **Program to interfaces** - Use interface references instead of concrete class references
4. **Use dependency injection** - Pass interfaces as constructor parameters
5. **Document interface contracts** - Clearly document what each method should do

#### **Common Mistakes to Avoid:**
1. ❌ Creating interfaces with too many methods
2. ❌ Implementing interface methods without proper validation
3. ❌ Not using aliases, leading to verbose code
4. ❌ Forgetting to implement all interface methods
5. ❌ Not handling interface reference casting properly

---

### **Final Challenge: Complete E-Commerce System**
**Combine all concepts learned:**
1. Create interfaces for `Product`, `ShoppingCart`, `Order`, `Payment`, `Shipping`
2. Implement multiple payment methods
3. Use events for order status updates
4. Create compound interfaces for different user types
5. Implement factory patterns for object creation
