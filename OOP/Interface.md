# **INTERFACES IN SAP ABAP**
## **The Complete Guide to ABAP Interfaces with Industry Practices**

---

## **TABLE OF CONTENTS**

1. [Introduction to Interfaces](#1-introduction-to-interfaces)
   - What is an Interface?
   - Real-World Analogy

2. [Why Do We Need Interfaces?](#2-why-do-we-need-interfaces)
   - Problems Without Interfaces
   - Solutions With Interfaces
   - Key Benefits

3. [Interface vs Abstract Class](#3-interface-vs-abstract-class)
   - Key Differences
   - When to Use Each
   - Comparison Table

4. [Defining an Interface](#4-defining-an-interface)
   - Interface Syntax
   - Methods and Parameters
   - Constants in Interfaces
   - Events in Interfaces
   - Complete Interface Example

5. [Implementing Interfaces in Classes](#5-implementing-interfaces-in-classes)
   - How to Implement an Interface
   - Rules for Implementation
   - Complete Example: Payment System
   - Best Practices

6. [Interface Components](#6-interface-components)
   - Methods in Interfaces
   - Parameters and Return Values
   - Constants and Types
   - Events
   - Practical Examples

7. [Multiple Interface Implementation](#7-multiple-interface-implementation)
   - Implementing Multiple Interfaces
   - Handling Conflicts
   - Real-World Example: Media Player
   - Advantages of Multiple Implementation
   - Exercise: Multi-Interface Implementation

8. [Interface Aliases](#8-interface-aliases)
   - What Are Aliases?
   - ALIASES Keyword
   - Why Use Aliases?
   - Practical Examples
   - Exercise: Creating Aliases

9. [Nested Interfaces (Compound Interfaces)](#9-nested-interfaces-compound-interfaces)
   - What Are Compound Interfaces?
   - Defining Compound Interfaces
   - Using Compound Interfaces
   - Real-World Example
   - Exercise

10. [Interface Reference Variables](#10-interface-reference-variables)
    - Polymorphism Through Interfaces
    - Creating Reference Variables
    - Using Interface References
    - Collections of Interface References
    - Complete Payment System Example

11. [Interface Constants and Types](#11-interface-constants-and-types)
    - Constants in Interfaces
    - Type Definitions in Interfaces
    - Using Interface Constants
    - Best Practices

12. [Events in Interfaces](#12-events-in-interfaces)
    - What Are Events?
    - Defining Events
    - Publishing Events
    - Subscribing to Events
    - Complete Example: Event-Driven System

13. [Industry Best Practices](#13-industry-best-practices)
    - Naming Conventions
    - Documentation Standards
    - Design Patterns with Interfaces
    - Performance Considerations
    - Common Mistakes to Avoid
    - Enterprise Architecture Practices

---

### **Table of Contents**


---

### **1 Introduction to Interfaces**

#### **What is an Interface?**
An interface is a contract or blueprint that defines **WHAT** a class must do, but not **HOW** it should do it. Think of it as a promise that any class implementing the interface will provide specific functionality.

```
╔═══════════════════════════════════════════════════════════════╗
║                    INTERFACE CONCEPT                          ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   INTERFACE = A Contract/Agreement                            ║
║                                                               ║
║   ┌─────────────────────────────────────────────────────┐     ║
║   │              INTERFACE: IF_PRINTABLE                │     ║
║   │                                                     │     ║
║   │   Contract: "Any class that implements me           │     ║
║   │              MUST have a PRINT method"              │     ║
║   │                                                     │     ║
║   │   Methods: PRINT( )  ← Only declaration, no code    │     ║
║   └─────────────────────────────────────────────────────┘     ║
║                           │                                   ║
║           ┌───────────────┼───────────────┐                   ║
║           ▼               ▼               ▼                   ║
║   ┌───────────┐   ┌───────────┐   ┌───────────┐               ║
║   │  REPORT   │   │  INVOICE  │   │  RECEIPT  │               ║
║   │   Class   │   │   Class   │   │   Class   │               ║
║   ├───────────┤   ├───────────┤   ├───────────┤               ║
║   │ PRINT()   │   │ PRINT()   │   │ PRINT()   │               ║
║   │ {own way} │   │ {own way} │   │ {own way} │               ║
║   └───────────┘   └───────────┘   └───────────┘               ║
║                                                               ║
║   Each class implements PRINT differently!                    ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Real-World Analogy**

```
╔═══════════════════════════════════════════════════════════════╗
║              REAL-WORLD ANALOGY: POWER SOCKET                 ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   INTERFACE = Power Socket Standard                           ║
║                                                               ║
║        ┌─────────┐                                            ║
║        │  ○   ○  │  ← Socket (Interface)                      ║
║        │    ○    │    "Any device with this plug pattern      ║
║        └─────────┘     can connect and get power"             ║
║                                                               ║
║   IMPLEMENTING CLASSES = Devices with matching plugs          ║
║                                                               ║
║   ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐       ║
║   │   TV    │   │  Phone  │   │ Laptop  │   │  Lamp   │       ║
║   │ Charger │   │ Charger │   │ Charger │   │         │       ║
║   └────┬────┘   └────┬────┘   └────┬────┘   └────┬────┘       ║
║        │             │             │             │            ║
║   All have the same plug pattern but work differently!        ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---

### **2 Why Do We Need Interfaces?**

#### **Problems Without Interfaces**

```
╔═══════════════════════════════════════════════════════════════╗
║           PROBLEM: WITHOUT INTERFACES                         ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   Scenario: Payment Processing System                         ║
║                                                               ║
║   ┌───────────────────────────────────────────────────────┐   ║
║   │                    MAIN PROGRAM                       │   ║
║   │                                                       │   ║
║   │  IF payment_type = 'CREDIT'.                          │   ║
║   │     CREATE OBJECT lo_credit.                          │   ║
║   │     lo_credit->process_credit_payment( ).             │   ║
║   │  ELSEIF payment_type = 'DEBIT'.                       │   ║
║   │     CREATE OBJECT lo_debit.                           │   ║
║   │     lo_debit->process_debit_payment( ).               │   ║
║   │  ELSEIF payment_type = 'UPI'.                         │   ║
║   │     CREATE OBJECT lo_upi.                             │   ║
║   │     lo_upi->process_upi_payment( ).                   │   ║
║   │  ELSEIF payment_type = 'WALLET'.                      │   ║
║   │     ...                                               │   ║
║   │  ENDIF.                                               │   ║
║   │                                                       │   ║
║   │  PROBLEMS:                                            │   ║
║   │  ✗ Too many IF conditions                             │   ║
║   │  ✗ Different method names for same purpose            │   ║
║   │  ✗ Adding new payment = changing main program         │   ║
║   │  ✗ No standard contract                               │   ║
║   └───────────────────────────────────────────────────────┘   ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Solution With Interfaces**

```
╔═══════════════════════════════════════════════════════════════╗
║           SOLUTION: WITH INTERFACES                           ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   ┌───────────────────────────────────────────────────────┐   ║
║   │           INTERFACE: IF_PAYMENT                       │   ║
║   │                                                       │   ║
║   │   METHODS: process_payment                            │   ║
║   │            get_transaction_id                         │   ║
║   │            validate_payment                           │   ║
║   └───────────────────────────────────────────────────────┘   ║
║                           │                                   ║
║       ┌───────────────────┼───────────────────┐               ║
║       ▼                   ▼                   ▼               ║
║   ┌─────────┐       ┌─────────┐        ┌─────────┐            ║
║   │ CREDIT  │       │  DEBIT  │        │   UPI   │            ║
║   │ PAYMENT │       │ PAYMENT │        │ PAYMENT │            ║
║   └─────────┘       └─────────┘        └─────────┘            ║
║                                                               ║
║   MAIN PROGRAM (Simple and Clean):                            ║
║   ┌───────────────────────────────────────────────────────┐   ║
║   │  DATA: lo_payment TYPE REF TO if_payment.             │   ║
║   │                                                       │   ║
║   │  lo_payment = factory->get_payment( payment_type ).   │   ║
║   │  lo_payment->process_payment( ).  " Same method!      │   ║
║   │                                                       │   ║
║   │  BENEFITS:                                            │   ║
║   │  ✓ One simple call works for ALL payment types        │   ║
║   │  ✓ Adding new payment = just create new class         │   ║
║   │  ✓ Main program never changes                         │   ║
║   │  ✓ Standard contract enforced                         │   ║
║   └───────────────────────────────────────────────────────┘   ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Key Benefits of Interfaces**

```
╔═══════════════════════════════════════════════════════════════╗
║              KEY BENEFITS OF INTERFACES                       ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   1. LOOSE COUPLING                                           ║
║      ┌─────────────────────────────────────────────────────┐  ║
║      │ Program depends on INTERFACE, not specific CLASS    │  ║
║      │ → Easy to swap implementations                      │  ║
║      └─────────────────────────────────────────────────────┘  ║
║                                                               ║
║   2. MULTIPLE INHERITANCE (Sort of)                           ║
║      ┌─────────────────────────────────────────────────────┐  ║
║      │ A class can implement MULTIPLE interfaces           │  ║
║      │ → Unlike single inheritance limitation              │  ║
║      └─────────────────────────────────────────────────────┘  ║
║                                                               ║
║   3. STANDARDIZATION                                          ║
║      ┌─────────────────────────────────────────────────────┐  ║
║      │ All implementing classes follow same structure      │  ║
║      │ → Consistent API across different implementations   │  ║
║      └─────────────────────────────────────────────────────┘  ║
║                                                               ║
║   4. TESTABILITY                                              ║
║      ┌─────────────────────────────────────────────────────┐  ║
║      │ Easy to create mock implementations for testing     │  ║
║      │ → Better unit testing capabilities                  │  ║
║      └─────────────────────────────────────────────────────┘  ║
║                                                               ║
║   5. FLEXIBILITY                                              ║
║      ┌─────────────────────────────────────────────────────┐  ║
║      │ Add new implementations without changing existing   │  ║
║      │ → Open/Closed principle                             │  ║
║      └─────────────────────────────────────────────────────┘  ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---

### **3 Interface vs Abstract Class**

#### **Comparison Table**

```
╔════════════════════════════════════════════════════════════════════════╗
║              INTERFACE vs ABSTRACT CLASS                               ║
╠════════════════════════════════════════════════════════════════════════╣
║                                                                        ║
║   ASPECT              │ INTERFACE           │ ABSTRACT CLASS           ║
║   ════════════════════╪═════════════════════╪══════════════════════════║
║                       │                     │                          ║
║   Methods             │ Only declarations   │ Both declarations        ║
║                       │ (no implementation) │ AND implementations      ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Instance Attributes │ NO                  │ YES                      ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Static Attributes   │ YES (Constants)     │ YES                      ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Constructor         │ NO                  │ YES                      ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Multiple            │ A class can         │ A class can inherit      ║
║   Implementation      │ implement MULTIPLE  │ only ONE abstract class  ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Visibility          │ Always PUBLIC       │ Can have PUBLIC,         ║
║                       │                     │ PROTECTED, PRIVATE       ║
║                       │                     │                          ║
║   ────────────────────┼─────────────────────┼──────────────────────────║
║                       │                     │                          ║
║   Use When            │ Defining a contract │ Sharing common code      ║
║                       │ for unrelated       │ among related classes    ║
║                       │ classes             │                          ║
║                       │                     │                          ║
╚════════════════════════════════════════════════════════════════════════╝
```

#### **Visual Comparison**

```
╔═══════════════════════════════════════════════════════════════╗
║              VISUAL COMPARISON                                ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   INTERFACE (Pure Contract)                                   ║
║   ┌─────────────────────────────────────────┐                 ║
║   │  INTERFACE if_flyable                   │                 ║
║   │  ├── fly( ) ────────► Empty/No code     │                 ║
║   │  ├── land( ) ───────► Empty/No code     │                 ║
║   │  └── get_altitude()─► Empty/No code     │                 ║
║   └─────────────────────────────────────────┘                 ║
║                                                               ║
║   ABSTRACT CLASS (Partial Implementation)                     ║
║   ┌─────────────────────────────────────────┐                 ║
║   │  ABSTRACT CLASS lcl_vehicle             │                 ║
║   │  ├── start( ) ──────► Has code ✓        │                 ║
║   │  ├── stop( ) ───────► Has code ✓        │                 ║
║   │  └── calculate_fuel()► ABSTRACT (empty) │                 ║
║   └─────────────────────────────────────────┘                 ║
║                                                               ║
║   KEY INSIGHT:                                                ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │ Interface = "WHAT to do" (100% contract)                │ ║
║   │ Abstract  = "WHAT to do" + "HOW to do some things"      │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **When to Use What?**

```
╔═══════════════════════════════════════════════════════════════╗
║              WHEN TO USE WHAT?                                ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   USE INTERFACE WHEN:                                         ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │ ✓ Unrelated classes need same behavior                  │ ║
║   │   Example: Bird, Airplane, Drone - all can FLY          │ ║
║   │            but they are NOT related                     │ ║
║   │                                                         │ ║
║   │ ✓ Multiple behaviors needed                             │ ║
║   │   Example: Document is Printable, Saveable, Shareable   │ ║
║   │                                                         │ ║
║   │ ✓ Defining API/Contract                                 │ ║
║   │   Example: All payment processors must have             │ ║
║   │            process_payment() method                     │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   USE ABSTRACT CLASS WHEN:                                    ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │ ✓ Related classes share common code                     │ ║
║   │   Example: Car, Truck, Motorcycle - all Vehicles        │ ║
║   │            Share: start(), stop()                       │ ║
║   │                                                         │ ║
║   │ ✓ Need to provide default implementation                │ ║
║   │   Example: Base calculation logic with                  │ ║
║   │            customizable parts                           │ ║
║   │                                                         │ ║
║   │ ✓ Need non-public members                               │ ║
║   │   Example: Protected helper methods                     │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---

### **4 Defining an Interface**

#### **Syntax for Interface Definition**

```abap
*&---------------------------------------------------------------------*
*& INTERFACE DEFINITION SYNTAX
*&---------------------------------------------------------------------*
*& An interface is defined using INTERFACE...ENDINTERFACE keywords
*& Interfaces can only have PUBLIC components
*& Interfaces CANNOT have implementation - only declarations
*&---------------------------------------------------------------------*

INTERFACE lif_interface_name.
  
  "---------------------------------------------------------------
  " CONSTANTS - Static values accessible via interface
  "---------------------------------------------------------------
  CONSTANTS: cv_constant_name TYPE datatype VALUE 'value'.
  
  "---------------------------------------------------------------
  " TYPES - Custom type definitions
  "---------------------------------------------------------------
  TYPES: ty_type_name TYPE datatype.
  
  "---------------------------------------------------------------
  " DATA - Only CLASS-DATA (Static) allowed, not instance data
  "---------------------------------------------------------------
  CLASS-DATA: gv_static_data TYPE datatype.
  
  "---------------------------------------------------------------
  " METHODS - Only declarations, NO implementation
  "---------------------------------------------------------------
  METHODS: method_name
    IMPORTING iv_param TYPE datatype
    EXPORTING ev_param TYPE datatype
    RETURNING VALUE(rv_result) TYPE datatype
    RAISING   cx_exception_class.
  
  "---------------------------------------------------------------
  " EVENTS - Event declarations
  "---------------------------------------------------------------
  EVENTS: event_name
    EXPORTING VALUE(ev_param) TYPE datatype.

ENDINTERFACE.
```

#### **First Interface Example**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_FIRST_EXAMPLE
*& Description: Basic interface definition and implementation
*&---------------------------------------------------------------------*
REPORT z_interface_first_example.

*----------------------------------------------------------------------*
* INTERFACE DEFINITION: IF_PRINTABLE
* Purpose: Contract for any object that can be printed
*----------------------------------------------------------------------*
INTERFACE lif_printable.
  
  "---------------------------------------------------------------
  " CONSTANTS
  " cv_default_copies: Default number of copies to print
  "---------------------------------------------------------------
  CONSTANTS: cv_default_copies TYPE i VALUE 1.
  
  "---------------------------------------------------------------
  " METHOD: print
  " Purpose: Print the object
  " Parameter: iv_copies - Number of copies to print
  "---------------------------------------------------------------
  METHODS: print
    IMPORTING iv_copies TYPE i DEFAULT cv_default_copies.
  
  "---------------------------------------------------------------
  " METHOD: get_print_preview
  " Purpose: Get preview of what will be printed
  " Returns: String containing preview text
  "---------------------------------------------------------------
  METHODS: get_print_preview
    RETURNING VALUE(rv_preview) TYPE string.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS DEFINITION: LCL_REPORT
* Purpose: A report document that can be printed
* Implements: LIF_PRINTABLE interface
*----------------------------------------------------------------------*
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    "-------------------------------------------------------------
    " Declare that this class implements the interface
    " This is a PROMISE that we will provide all interface methods
    "-------------------------------------------------------------
    INTERFACES: lif_printable.
    
    "-------------------------------------------------------------
    " Constructor to set report details
    "-------------------------------------------------------------
    METHODS: constructor
      IMPORTING iv_title   TYPE string
                iv_content TYPE string.
  
  PRIVATE SECTION.
    DATA: mv_title   TYPE string,
          mv_content TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION: LCL_REPORT
*----------------------------------------------------------------------*
CLASS lcl_report IMPLEMENTATION.

  "-------------------------------------------------------------------
  " Constructor: Initialize report with title and content
  "-------------------------------------------------------------------
  METHOD constructor.
    mv_title   = iv_title.
    mv_content = iv_content.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " Interface method: print
  " Note: Must use full name - lif_printable~print
  " The ~ (tilde) connects interface name with method name
  "-------------------------------------------------------------------
  METHOD lif_printable~print.
    
    DATA: lv_counter TYPE i.
    
    "---------------------------------------------------------------
    " Print the report specified number of times
    "---------------------------------------------------------------
    DO iv_copies TIMES.
      lv_counter = lv_counter + 1.
      
      WRITE: / '================================================'.
      WRITE: / 'REPORT PRINT - Copy', lv_counter, 'of', iv_copies.
      WRITE: / '================================================'.
      WRITE: / 'Title:', mv_title.
      WRITE: / 'Content:', mv_content.
      WRITE: / '================================================'.
      SKIP.
    ENDDO.
    
  ENDMETHOD.

  "-------------------------------------------------------------------
  " Interface method: get_print_preview
  "-------------------------------------------------------------------
  METHOD lif_printable~get_print_preview.
    
    "---------------------------------------------------------------
    " Return a preview of the report
    "---------------------------------------------------------------
    rv_preview = |Report Preview: { mv_title } - { mv_content }|.
    
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS DEFINITION: LCL_INVOICE
* Purpose: An invoice document that can be printed
* Implements: LIF_PRINTABLE interface (same as report)
*----------------------------------------------------------------------*
CLASS lcl_invoice DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_printable.
    
    METHODS: constructor
      IMPORTING iv_invoice_no TYPE string
                iv_amount     TYPE p.
  
  PRIVATE SECTION.
    DATA: mv_invoice_no TYPE string,
          mv_amount     TYPE p DECIMALS 2.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION: LCL_INVOICE
*----------------------------------------------------------------------*
CLASS lcl_invoice IMPLEMENTATION.

  METHOD constructor.
    mv_invoice_no = iv_invoice_no.
    mv_amount     = iv_amount.
  ENDMETHOD.

  "-------------------------------------------------------------------
  " Same interface, but DIFFERENT implementation
  " Invoice prints differently than Report!
  "-------------------------------------------------------------------
  METHOD lif_printable~print.
    
    DO iv_copies TIMES.
      WRITE: / '################################################'.
      WRITE: / '#          INVOICE DOCUMENT                    #'.
      WRITE: / '################################################'.
      WRITE: / '# Invoice No:', mv_invoice_no.
      WRITE: / '# Amount    : $', mv_amount.
      WRITE: / '################################################'.
      SKIP.
    ENDDO.
    
  ENDMETHOD.

  METHOD lif_printable~get_print_preview.
    rv_preview = |Invoice #{ mv_invoice_no } - Amount: ${ mv_amount }|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_report  TYPE REF TO lcl_report,
        lo_invoice TYPE REF TO lcl_invoice.

  " Create objects
  CREATE OBJECT lo_report
    EXPORTING
      iv_title   = 'Sales Report Q4'
      iv_content = 'Total sales: $1,500,000'.

  CREATE OBJECT lo_invoice
    EXPORTING
      iv_invoice_no = 'INV-2024-001'
      iv_amount     = '2500.00'.

  " Print report (2 copies)
  lo_report->lif_printable~print( iv_copies = 2 ).

  " Print invoice (1 copy - using default)
  lo_invoice->lif_printable~print( ).
```

#### **Output:**

```
================================================
REPORT PRINT - Copy 1 of 2
================================================
Title: Sales Report Q4
Content: Total sales: $1,500,000
================================================

================================================
REPORT PRINT - Copy 2 of 2
================================================
Title: Sales Report Q4
Content: Total sales: $1,500,000
================================================

################################################
#          INVOICE DOCUMENT                    #
################################################
# Invoice No: INV-2024-001
# Amount    : $ 2,500.00
################################################
```

---

### **5 Implementing Interfaces in Classes**

#### **The INTERFACES Statement**

```
╔═══════════════════════════════════════════════════════════════╗
║              IMPLEMENTING INTERFACES                          ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   SYNTAX:                                                     ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │  CLASS lcl_myclass DEFINITION.                          │ ║
║   │    PUBLIC SECTION.                                      │ ║
║   │      INTERFACES: lif_interface1,                        │ ║
║   │                  lif_interface2.                        │ ║
║   │  ENDCLASS.                                              │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   RULES:                                                      ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │ 1. INTERFACES statement must be in PUBLIC SECTION       │ ║
║   │                                                         │ ║
║   │ 2. ALL methods of interface MUST be implemented         │ ║
║   │                                                         │ ║
║   │ 3. Interface methods are accessed with ~                │ ║
║   │    Example: lif_interface~method_name                   │ ║
║   │                                                         │ ║
║   │ 4. Multiple interfaces can be implemented               │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Accessing Interface Components**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_ACCESS_METHODS
*& Description: Different ways to access interface components
*&---------------------------------------------------------------------*
REPORT z_interface_access_methods.

*----------------------------------------------------------------------*
* INTERFACE DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_calculator.
  
  CONSTANTS: cv_pi TYPE p DECIMALS 5 VALUE '3.14159'.
  
  METHODS: add
    IMPORTING iv_num1 TYPE i
              iv_num2 TYPE i
    RETURNING VALUE(rv_result) TYPE i.
  
  METHODS: multiply
    IMPORTING iv_num1 TYPE i
              iv_num2 TYPE i
    RETURNING VALUE(rv_result) TYPE i.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_simple_calc DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_calculator.
    
    "-------------------------------------------------------------------
    " ALIASES - Create short names for interface methods
    " This is optional but very convenient!
    "-------------------------------------------------------------------
    ALIASES: add      FOR lif_calculator~add,
             multiply FOR lif_calculator~multiply,
             pi       FOR lif_calculator~cv_pi.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_simple_calc IMPLEMENTATION.

  METHOD lif_calculator~add.
    rv_result = iv_num1 + iv_num2.
  ENDMETHOD.

  METHOD lif_calculator~multiply.
    rv_result = iv_num1 * iv_num2.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_calc TYPE REF TO lcl_simple_calc,
        lv_result TYPE i.

  CREATE OBJECT lo_calc.

  "---------------------------------------------------------------------
  " WAY 1: Using full interface path (Always works)
  "---------------------------------------------------------------------
  lv_result = lo_calc->lif_calculator~add( iv_num1 = 10 
                                           iv_num2 = 20 ).
  WRITE: / 'Way 1 - Full path: 10 + 20 =', lv_result.

  "---------------------------------------------------------------------
  " WAY 2: Using ALIAS (Shorter and cleaner - if alias defined)
  "---------------------------------------------------------------------
  lv_result = lo_calc->add( iv_num1 = 10 
                            iv_num2 = 20 ).
  WRITE: / 'Way 2 - Alias: 10 + 20 =', lv_result.

  "---------------------------------------------------------------------
  " Accessing interface constant via full path
  "---------------------------------------------------------------------
  WRITE: / 'PI value (full path):', lo_calc->lif_calculator~cv_pi.

  "---------------------------------------------------------------------
  " Accessing interface constant via alias
  "---------------------------------------------------------------------
  WRITE: / 'PI value (alias):', lo_calc->pi.

  "---------------------------------------------------------------------
  " Accessing interface constant directly from interface (static)
  "---------------------------------------------------------------------
  WRITE: / 'PI value (direct):', lif_calculator=>cv_pi.
```

#### **Output:**

```
Way 1 - Full path: 10 + 20 = 30
Way 2 - Alias: 10 + 20 = 30
PI value (full path): 3.14159
PI value (alias): 3.14159
PI value (direct): 3.14159
```

---

### **6 Interface Components**

#### **Complete Interface with All Component Types**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_ALL_COMPONENTS
*& Description: Interface with all possible component types
*&---------------------------------------------------------------------*
REPORT z_interface_all_components.

*----------------------------------------------------------------------*
* INTERFACE DEFINITION: LIF_COMPLETE
* Purpose: Demonstrate all types of interface components
*----------------------------------------------------------------------*
INTERFACE lif_complete.

  "=====================================================================
  " 1. TYPES - Custom type definitions
  " Purpose: Define reusable data types for implementing classes
  "=====================================================================
  TYPES:
    "-------------------------------------------------------------------
    " Simple type definition
    "-------------------------------------------------------------------
    ty_status TYPE c LENGTH 10,
    
    "-------------------------------------------------------------------
    " Structure type definition
    "-------------------------------------------------------------------
    BEGIN OF ty_address,
      street  TYPE string,
      city    TYPE string,
      zipcode TYPE string,
      country TYPE string,
    END OF ty_address,
    
    "-------------------------------------------------------------------
    " Table type definition
    "-------------------------------------------------------------------
    ty_t_address TYPE STANDARD TABLE OF ty_address WITH DEFAULT KEY.

  "=====================================================================
  " 2. CONSTANTS - Fixed values
  " Purpose: Provide standard values that don't change
  "=====================================================================
  CONSTANTS:
    "-------------------------------------------------------------------
    " Simple constants
    "-------------------------------------------------------------------
    cv_max_length    TYPE i VALUE 100,
    cv_default_status TYPE ty_status VALUE 'ACTIVE',
    
    "-------------------------------------------------------------------
    " Constants for status codes
    "-------------------------------------------------------------------
    cv_status_active   TYPE ty_status VALUE 'ACTIVE',
    cv_status_inactive TYPE ty_status VALUE 'INACTIVE',
    cv_status_pending  TYPE ty_status VALUE 'PENDING'.

  "=====================================================================
  " 3. CLASS-DATA - Static data (shared across all implementations)
  " Note: Only CLASS-DATA allowed, not instance DATA
  "=====================================================================
  CLASS-DATA:
    gv_instance_count TYPE i,
    gv_last_error     TYPE string.

  "=====================================================================
  " 4. METHODS - Method declarations
  " Purpose: Define the contract - what methods must be implemented
  "=====================================================================
  METHODS:
    "-------------------------------------------------------------------
    " Method with IMPORTING parameters
    "-------------------------------------------------------------------
    set_data
      IMPORTING
        iv_id   TYPE string
        is_data TYPE ty_address,
    
    "-------------------------------------------------------------------
    " Method with RETURNING value
    "-------------------------------------------------------------------
    get_status
      RETURNING VALUE(rv_status) TYPE ty_status,
    
    "-------------------------------------------------------------------
    " Method with EXPORTING parameters
    "-------------------------------------------------------------------
    get_all_data
      EXPORTING
        ev_id      TYPE string
        es_address TYPE ty_address
        et_history TYPE ty_t_address,
    
    "-------------------------------------------------------------------
    " Method with CHANGING parameter
    "-------------------------------------------------------------------
    process_data
      CHANGING
        cs_data TYPE ty_address,
    
    "-------------------------------------------------------------------
    " Method with RAISING (can throw exception)
    "-------------------------------------------------------------------
    validate
      RAISING
        cx_sy_arithmetic_error.

  "=====================================================================
  " 5. EVENTS - Event declarations
  " Purpose: Define events that can be raised by implementing classes
  "=====================================================================
  EVENTS:
    "-------------------------------------------------------------------
    " Event without parameters
    "-------------------------------------------------------------------
    status_changed,
    
    "-------------------------------------------------------------------
    " Event with parameters
    "-------------------------------------------------------------------
    data_updated
      EXPORTING
        VALUE(ev_old_status) TYPE ty_status
        VALUE(ev_new_status) TYPE ty_status.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTING THE INTERFACE
*----------------------------------------------------------------------*
CLASS lcl_customer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_complete.
    
    " Aliases for convenience
    ALIASES: set_data   FOR lif_complete~set_data,
             get_status FOR lif_complete~get_status.
    
    METHODS: constructor
      IMPORTING iv_name TYPE string.

  PRIVATE SECTION.
    DATA: mv_id      TYPE string,
          mv_name    TYPE string,
          ms_address TYPE lif_complete=>ty_address,      " Using interface type
          mv_status  TYPE lif_complete=>ty_status.       " Using interface type
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_customer IMPLEMENTATION.

  METHOD constructor.
    mv_name = iv_name.
    mv_status = lif_complete=>cv_status_active.  " Using interface constant
    
    " Increment instance counter (static data)
    lif_complete=>gv_instance_count = lif_complete=>gv_instance_count + 1.
  ENDMETHOD.

  METHOD lif_complete~set_data.
    mv_id = iv_id.
    ms_address = is_data.
  ENDMETHOD.

  METHOD lif_complete~get_status.
    rv_status = mv_status.
  ENDMETHOD.

  METHOD lif_complete~get_all_data.
    ev_id = mv_id.
    es_address = ms_address.
    " et_history would be populated from history table
  ENDMETHOD.

  METHOD lif_complete~process_data.
    " Modify the passed structure
    cs_data-city = to_upper( cs_data-city ).
    cs_data-country = to_upper( cs_data-country ).
  ENDMETHOD.

  METHOD lif_complete~validate.
    IF mv_id IS INITIAL.
      " In real scenario, would raise proper exception
      lif_complete=>gv_last_error = 'ID is required'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_cust1     TYPE REF TO lcl_customer,
        lo_cust2     TYPE REF TO lcl_customer,
        ls_address   TYPE lif_complete=>ty_address,
        lv_status    TYPE lif_complete=>ty_status.

  " Create customer objects
  CREATE OBJECT lo_cust1 EXPORTING iv_name = 'John Doe'.
  CREATE OBJECT lo_cust2 EXPORTING iv_name = 'Jane Smith'.

  " Set address using interface type
  ls_address-street  = '123 Main St'.
  ls_address-city    = 'New York'.
  ls_address-zipcode = '10001'.
  ls_address-country = 'USA'.

  lo_cust1->set_data( iv_id   = 'CUST001'
                      is_data = ls_address ).

  " Get status
  lv_status = lo_cust1->get_status( ).

  " Display results
  WRITE: / 'Customer 1 Status:', lv_status.
  WRITE: / 'Total instances created:', lif_complete=>gv_instance_count.
  WRITE: / 'Max length constant:', lif_complete=>cv_max_length.
  WRITE: / 'Active status constant:', lif_complete=>cv_status_active.
```

#### **Output:**

```
Customer 1 Status: ACTIVE
Total instances created: 2
Max length constant: 100
Active status constant: ACTIVE
```

---

### **7 Multiple Interface Implementation**

#### **Implementing Multiple Interfaces**

```
╔═══════════════════════════════════════════════════════════════╗
║         MULTIPLE INTERFACE IMPLEMENTATION                     ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   A class can implement MULTIPLE interfaces                   ║
║   This provides a form of "multiple inheritance"              ║
║                                                               ║
║                    ┌──────────────┐                           ║
║                    │   DOCUMENT   │                           ║
║                    │    CLASS     │                           ║
║                    └──────────────┘                           ║
║                           │                                   ║
║          ┌────────────────┼────────────────┐                  ║
║          ▼                ▼                ▼                  ║
║   ┌────────────┐   ┌────────────┐   ┌────────────┐            ║
║   │IF_PRINTABLE│   │IF_SAVEABLE │   │IF_SHAREABLE│            ║
║   │            │   │            │   │            │            ║
║   │ print()    │   │ save()     │   │ share()    │            ║
║   │ preview()  │   │ load()     │   │ get_link() │            ║
║   └────────────┘   └────────────┘   └────────────┘            ║
║                                                               ║
║   Document class can:                                         ║
║   ✓ Be printed (from IF_PRINTABLE)                            ║
║   ✓ Be saved/loaded (from IF_SAVEABLE)                        ║
║   ✓ Be shared (from IF_SHAREABLE)                             ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Complete Example**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_MULTIPLE_INTERFACES
*& Description: Class implementing multiple interfaces
*&---------------------------------------------------------------------*
REPORT z_multiple_interfaces.

*----------------------------------------------------------------------*
* INTERFACE 1: IF_PRINTABLE
*----------------------------------------------------------------------*
INTERFACE lif_printable.
  METHODS:
    print,
    get_preview RETURNING VALUE(rv_preview) TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* INTERFACE 2: IF_SAVEABLE
*----------------------------------------------------------------------*
INTERFACE lif_saveable.
  METHODS:
    save RETURNING VALUE(rv_success) TYPE abap_bool,
    load IMPORTING iv_id TYPE string RETURNING VALUE(rv_success) TYPE abap_bool,
    get_last_saved RETURNING VALUE(rv_timestamp) TYPE timestamp.
ENDINTERFACE.

*----------------------------------------------------------------------*
* INTERFACE 3: IF_SHAREABLE
*----------------------------------------------------------------------*
INTERFACE lif_shareable.
  CONSTANTS:
    cv_share_public  TYPE c LENGTH 1 VALUE 'P',
    cv_share_private TYPE c LENGTH 1 VALUE 'V',
    cv_share_link    TYPE c LENGTH 1 VALUE 'L'.
    
  METHODS:
    share IMPORTING iv_mode TYPE c RETURNING VALUE(rv_link) TYPE string,
    get_share_count RETURNING VALUE(rv_count) TYPE i.
ENDINTERFACE.

*----------------------------------------------------------------------*
* INTERFACE 4: IF_VERSIONED
*----------------------------------------------------------------------*
INTERFACE lif_versioned.
  TYPES:
    BEGIN OF ty_version,
      version_no TYPE i,
      timestamp  TYPE timestamp,
      author     TYPE string,
    END OF ty_version,
    ty_t_versions TYPE STANDARD TABLE OF ty_version WITH DEFAULT KEY.
    
  METHODS:
    get_version RETURNING VALUE(rv_version) TYPE i,
    get_version_history RETURNING VALUE(rt_versions) TYPE ty_t_versions,
    create_new_version RETURNING VALUE(rv_new_version) TYPE i.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: LCL_DOCUMENT
* Implements ALL four interfaces!
*----------------------------------------------------------------------*
CLASS lcl_document DEFINITION.
  PUBLIC SECTION.
    "-------------------------------------------------------------------
    " Implement multiple interfaces
    "-------------------------------------------------------------------
    INTERFACES:
      lif_printable,
      lif_saveable,
      lif_shareable,
      lif_versioned.
    
    "-------------------------------------------------------------------
    " Create aliases for cleaner access
    "-------------------------------------------------------------------
    ALIASES:
      " Printable aliases
      print       FOR lif_printable~print,
      get_preview FOR lif_printable~get_preview,
      " Saveable aliases
      save        FOR lif_saveable~save,
      load        FOR lif_saveable~load,
      " Shareable aliases
      share       FOR lif_shareable~share,
      " Versioned aliases
      get_version FOR lif_versioned~get_version.
    
    "-------------------------------------------------------------------
    " Own methods
    "-------------------------------------------------------------------
    METHODS:
      constructor
        IMPORTING
          iv_title   TYPE string
          iv_content TYPE string,
      set_content
        IMPORTING
          iv_content TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_title       TYPE string,
      mv_content     TYPE string,
      mv_last_saved  TYPE timestamp,
      mv_version     TYPE i VALUE 1,
      mv_share_count TYPE i VALUE 0,
      mt_versions    TYPE lif_versioned=>ty_t_versions.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_document IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_version TYPE lif_versioned=>ty_version.
    
    mv_title   = iv_title.
    mv_content = iv_content.
    
    " Record initial version
    ls_version-version_no = 1.
    GET TIME STAMP FIELD ls_version-timestamp.
    ls_version-author = 'System'.
    APPEND ls_version TO mt_versions.
  ENDMETHOD.

  METHOD set_content.
    mv_content = iv_content.
  ENDMETHOD.

  "=====================================================================
  " INTERFACE: LIF_PRINTABLE - Implementation
  "=====================================================================
  
  METHOD lif_printable~print.
    WRITE: / '╔══════════════════════════════════════════════╗'.
    WRITE: / '║              DOCUMENT PRINTOUT               ║'.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    WRITE: / '║ Title:', mv_title.
    WRITE: / '║ Version:', mv_version.
    WRITE: / '╠══════════════════════════════════════════════╣'.
    WRITE: / '║ Content:'.
    WRITE: / '║', mv_content.
    WRITE: / '╚══════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD lif_printable~get_preview.
    rv_preview = |Preview: { mv_title } (v{ mv_version })|.
  ENDMETHOD.

  "=====================================================================
  " INTERFACE: LIF_SAVEABLE - Implementation
  "=====================================================================
  
  METHOD lif_saveable~save.
    " Simulate saving to database
    GET TIME STAMP FIELD mv_last_saved.
    rv_success = abap_true.
    WRITE: / 'Document saved successfully at:', mv_last_saved.
  ENDMETHOD.

  METHOD lif_saveable~load.
    " Simulate loading from database
    IF iv_id IS NOT INITIAL.
      rv_success = abap_true.
      WRITE: / 'Document loaded:', iv_id.
    ELSE.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD lif_saveable~get_last_saved.
    rv_timestamp = mv_last_saved.
  ENDMETHOD.

  "=====================================================================
  " INTERFACE: LIF_SHAREABLE - Implementation
  "=====================================================================
  
  METHOD lif_shareable~share.
    mv_share_count = mv_share_count + 1.
    
    CASE iv_mode.
      WHEN lif_shareable=>cv_share_public.
        rv_link = |https://docs.example.com/public/{ mv_title }|.
      WHEN lif_shareable=>cv_share_private.
        rv_link = |https://docs.example.com/private/{ mv_title }?token=xyz|.
      WHEN lif_shareable=>cv_share_link.
        rv_link = |https://docs.example.com/link/{ mv_title }?code=abc123|.
      WHEN OTHERS.
        rv_link = 'Invalid share mode'.
    ENDCASE.
    
    WRITE: / 'Document shared. Link:', rv_link.
  ENDMETHOD.

  METHOD lif_shareable~get_share_count.
    rv_count = mv_share_count.
  ENDMETHOD.

  "=====================================================================
  " INTERFACE: LIF_VERSIONED - Implementation
  "=====================================================================
  
  METHOD lif_versioned~get_version.
    rv_version = mv_version.
  ENDMETHOD.

  METHOD lif_versioned~get_version_history.
    rt_versions = mt_versions.
  ENDMETHOD.

  METHOD lif_versioned~create_new_version.
    DATA: ls_version TYPE lif_versioned=>ty_version.
    
    mv_version = mv_version + 1.
    
    ls_version-version_no = mv_version.
    GET TIME STAMP FIELD ls_version-timestamp.
    ls_version-author = 'User'.
    APPEND ls_version TO mt_versions.
    
    rv_new_version = mv_version.
    
    WRITE: / 'New version created:', mv_version.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_doc       TYPE REF TO lcl_document,
        lv_link      TYPE string,
        lv_version   TYPE i,
        lt_versions  TYPE lif_versioned=>ty_t_versions.

  " Create document
  CREATE OBJECT lo_doc
    EXPORTING
      iv_title   = 'Project Report'
      iv_content = 'This is the project status report for Q4 2024.'.

  WRITE: / '========================================'.
  WRITE: / 'TESTING MULTIPLE INTERFACES'.
  WRITE: / '========================================'.
  SKIP.

  " Test IF_PRINTABLE
  WRITE: / '--- Testing IF_PRINTABLE ---'.
  lo_doc->print( ).
  WRITE: / 'Preview:', lo_doc->get_preview( ).
  SKIP.

  " Test IF_SAVEABLE
  WRITE: / '--- Testing IF_SAVEABLE ---'.
  lo_doc->save( ).
  SKIP.

  " Test IF_SHAREABLE
  WRITE: / '--- Testing IF_SHAREABLE ---'.
  lv_link = lo_doc->share( iv_mode = lif_shareable=>cv_share_public ).
  SKIP.

  " Test IF_VERSIONED
  WRITE: / '--- Testing IF_VERSIONED ---'.
  lv_version = lo_doc->get_version( ).
  WRITE: / 'Current version:', lv_version.
  
  lo_doc->lif_versioned~create_new_version( ).
  lv_version = lo_doc->get_version( ).
  WRITE: / 'New version:', lv_version.
```

#### **Output:**

```
========================================
TESTING MULTIPLE INTERFACES
========================================

--- Testing IF_PRINTABLE ---
╔══════════════════════════════════════════════╗
║              DOCUMENT PRINTOUT               ║
╠══════════════════════════════════════════════╣
║ Title: Project Report
║ Version: 1
╠══════════════════════════════════════════════╣
║ Content:
║ This is the project status report for Q4 2024.
╚══════════════════════════════════════════════╝
Preview: Preview: Project Report (v1)

--- Testing IF_SAVEABLE ---
Document saved successfully at: 20241215123456

--- Testing IF_SHAREABLE ---
Document shared. Link: https://docs.example.com/public/Project Report

--- Testing IF_VERSIONED ---
Current version: 1
New version created: 2
New version: 2
```

---

### **8 Interface Aliases**

#### **Understanding Aliases**

```
╔═══════════════════════════════════════════════════════════════╗
║                    INTERFACE ALIASES                          ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   PROBLEM: Interface methods have long names                  ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │  lo_obj->lif_printable~print( ).                        │ ║
║   │  lo_obj->lif_saveable~save( ).                          │ ║
║   │  lo_obj->lif_shareable~share( ).                        │ ║
║   │                                                         │ ║
║   │  ↑ Too verbose and hard to read!                        │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   SOLUTION: Use ALIASES                                       ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │  ALIASES: print FOR lif_printable~print.                │ ║
║   │                                                         │ ║
║   │  Now you can simply call:                               │ ║
║   │  lo_obj->print( ).  ← Much cleaner!                     │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Alias Syntax and Examples**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_ALIASES
*& Description: Comprehensive examples of interface aliases
*&---------------------------------------------------------------------*
REPORT z_interface_aliases.

*----------------------------------------------------------------------*
* INTERFACE DEFINITIONS
*----------------------------------------------------------------------*
INTERFACE lif_readable.
  METHODS: read RETURNING VALUE(rv_data) TYPE string.
ENDINTERFACE.

INTERFACE lif_writable.
  METHODS: write IMPORTING iv_data TYPE string.
ENDINTERFACE.

INTERFACE lif_deletable.
  METHODS: delete RETURNING VALUE(rv_success) TYPE abap_bool.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS WITHOUT ALIASES (Verbose Access)
*----------------------------------------------------------------------*
CLASS lcl_file_no_alias DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_readable,
                lif_writable,
                lif_deletable.
                
    " NO aliases defined - must use full interface path
    
    METHODS: constructor IMPORTING iv_name TYPE string.
    
  PRIVATE SECTION.
    DATA: mv_name    TYPE string,
          mv_content TYPE string.
ENDCLASS.

CLASS lcl_file_no_alias IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  
  METHOD lif_readable~read.
    rv_data = mv_content.
  ENDMETHOD.
  
  METHOD lif_writable~write.
    mv_content = iv_data.
  ENDMETHOD.
  
  METHOD lif_deletable~delete.
    CLEAR: mv_content.
    rv_success = abap_true.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS WITH ALIASES (Clean Access)
*----------------------------------------------------------------------*
CLASS lcl_file_with_alias DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_readable,
                lif_writable,
                lif_deletable.
    
    "-------------------------------------------------------------------
    " ALIASES - Create short names for interface methods
    " Syntax: ALIASES short_name FOR interface~method_name
    "-------------------------------------------------------------------
    ALIASES:
      " Simple alias - same name as interface method
      read   FOR lif_readable~read,
      write  FOR lif_writable~write,
      delete FOR lif_deletable~delete.
    
    METHODS: constructor IMPORTING iv_name TYPE string.
    
  PRIVATE SECTION.
    DATA: mv_name    TYPE string,
          mv_content TYPE string.
ENDCLASS.

CLASS lcl_file_with_alias IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  
  METHOD lif_readable~read.
    rv_data = |File: { mv_name } - Content: { mv_content }|.
  ENDMETHOD.
  
  METHOD lif_writable~write.
    mv_content = iv_data.
  ENDMETHOD.
  
  METHOD lif_deletable~delete.
    CLEAR: mv_content.
    rv_success = abap_true.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS WITH RENAMED ALIASES
*----------------------------------------------------------------------*
CLASS lcl_database_record DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_readable,
                lif_writable,
                lif_deletable.
    
    "-------------------------------------------------------------------
    " ALIASES with DIFFERENT names
    " You can give any name to the alias
    "-------------------------------------------------------------------
    ALIASES:
      " Renamed to be more specific for database context
      fetch  FOR lif_readable~read,      " 'fetch' instead of 'read'
      update FOR lif_writable~write,     " 'update' instead of 'write'
      remove FOR lif_deletable~delete.   " 'remove' instead of 'delete'
    
    METHODS: constructor IMPORTING iv_id TYPE string.
    
  PRIVATE SECTION.
    DATA: mv_id   TYPE string,
          mv_data TYPE string.
ENDCLASS.

CLASS lcl_database_record IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.
  
  METHOD lif_readable~read.
    rv_data = |DB Record ID: { mv_id } - Data: { mv_data }|.
  ENDMETHOD.
  
  METHOD lif_writable~write.
    mv_data = iv_data.
  ENDMETHOD.
  
  METHOD lif_deletable~delete.
    CLEAR: mv_data.
    rv_success = abap_true.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM - Demonstrate Access Methods
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_file_no_alias   TYPE REF TO lcl_file_no_alias,
        lo_file_with_alias TYPE REF TO lcl_file_with_alias,
        lo_db_record       TYPE REF TO lcl_database_record,
        lv_data            TYPE string.

  " Create objects
  CREATE OBJECT lo_file_no_alias EXPORTING iv_name = 'report.txt'.
  CREATE OBJECT lo_file_with_alias EXPORTING iv_name = 'document.pdf'.
  CREATE OBJECT lo_db_record EXPORTING iv_id = 'REC001'.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'WITHOUT ALIASES - Verbose Access'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  
  " Must use full interface path
  lo_file_no_alias->lif_writable~write( 'Hello World' ).
  lv_data = lo_file_no_alias->lif_readable~read( ).
  WRITE: / lv_data.
  SKIP.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'WITH ALIASES - Clean Access'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  
  " Can use short alias name
  lo_file_with_alias->write( 'Content via alias' ).   " Clean!
  lv_data = lo_file_with_alias->read( ).              " Clean!
  WRITE: / lv_data.
  SKIP.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'WITH RENAMED ALIASES - Domain-Specific Names'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  
  " Using renamed aliases
  lo_db_record->update( 'Database value' ).   " 'update' instead of 'write'
  lv_data = lo_db_record->fetch( ).           " 'fetch' instead of 'read'
  WRITE: / lv_data.
  
  IF lo_db_record->remove( ) = abap_true.     " 'remove' instead of 'delete'
    WRITE: / 'Record removed successfully'.
  ENDIF.
```

#### **Output:**

```
═══════════════════════════════════════════════════════
WITHOUT ALIASES - Verbose Access
═══════════════════════════════════════════════════════
Hello World

═══════════════════════════════════════════════════════
WITH ALIASES - Clean Access
═══════════════════════════════════════════════════════
File: document.pdf - Content: Content via alias

═══════════════════════════════════════════════════════
WITH RENAMED ALIASES - Domain-Specific Names
═══════════════════════════════════════════════════════
DB Record ID: REC001 - Data: Database value
Record removed successfully
```

---

### **9 Nested Interfaces (Compound Interfaces)**

#### **What are Nested Interfaces?**

```
╔═══════════════════════════════════════════════════════════════╗
║              NESTED/COMPOUND INTERFACES                       ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   An interface can INCLUDE other interfaces                   ║
║   This creates an "interface hierarchy"                       ║
║                                                               ║
║   ┌─────────────────┐   ┌─────────────────┐                   ║
║   │  IF_READABLE    │   │  IF_WRITABLE    │                   ║
║   │  ├── read()     │   │  ├── write()    │                   ║
║   └────────┬────────┘   └────────┬────────┘                   ║
║            │                     │                            ║
║            └──────────┬──────────┘                            ║
║                       ▼                                       ║
║            ┌─────────────────────┐                            ║
║            │     IF_FILE         │                            ║
║            │ ┌─────────────────┐ │                            ║
║            │ │ INCLUDES:       │ │                            ║
║            │ │ - IF_READABLE   │ │                            ║
║            │ │ - IF_WRITABLE   │ │                            ║
║            │ └─────────────────┘ │                            ║
║            │ OWN METHODS:        │                            ║
║            │ ├── open()          │                            ║
║            │ ├── close()         │                            ║
║            └─────────────────────┘                            ║
║                                                               ║
║   A class implementing IF_FILE must implement:                ║
║   ✓ read()   (from IF_READABLE)                               ║
║   ✓ write()  (from IF_WRITABLE)                               ║
║   ✓ open()   (from IF_FILE)                                   ║
║   ✓ close()  (from IF_FILE)                                   ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Complete Example**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_NESTED_INTERFACES
*& Description: Interfaces including other interfaces
*&---------------------------------------------------------------------*
REPORT z_nested_interfaces.

*----------------------------------------------------------------------*
* BASE INTERFACES
*----------------------------------------------------------------------*

" Interface for identifiable entities
INTERFACE lif_identifiable.
  METHODS: get_id RETURNING VALUE(rv_id) TYPE string.
ENDINTERFACE.

" Interface for timestamped entities
INTERFACE lif_timestamped.
  METHODS:
    get_created_at RETURNING VALUE(rv_timestamp) TYPE timestamp,
    get_updated_at RETURNING VALUE(rv_timestamp) TYPE timestamp.
ENDINTERFACE.

" Interface for auditable entities
INTERFACE lif_auditable.
  METHODS:
    get_created_by RETURNING VALUE(rv_user) TYPE string,
    get_updated_by RETURNING VALUE(rv_user) TYPE string.
ENDINTERFACE.

" Interface for serializable entities
INTERFACE lif_serializable.
  METHODS:
    to_json RETURNING VALUE(rv_json) TYPE string,
    to_xml RETURNING VALUE(rv_xml) TYPE string.
ENDINTERFACE.

*----------------------------------------------------------------------*
* COMPOUND INTERFACE - Combines Multiple Interfaces
*----------------------------------------------------------------------*
INTERFACE lif_business_entity.
  "---------------------------------------------------------------------
  " INTERFACES statement - Include other interfaces
  " This interface now contains ALL methods from included interfaces
  "---------------------------------------------------------------------
  INTERFACES:
    lif_identifiable,
    lif_timestamped,
    lif_auditable,
    lif_serializable.
  
  "---------------------------------------------------------------------
  " Additional methods specific to this interface
  "---------------------------------------------------------------------
  METHODS:
    validate RETURNING VALUE(rv_valid) TYPE abap_bool,
    get_display_name RETURNING VALUE(rv_name) TYPE string.

ENDINTERFACE.

*----------------------------------------------------------------------*
* ANOTHER COMPOUND INTERFACE - Different Combination
*----------------------------------------------------------------------*
INTERFACE lif_persistable.
  "---------------------------------------------------------------------
  " Include only identity and timestamp
  "---------------------------------------------------------------------
  INTERFACES:
    lif_identifiable,
    lif_timestamped.
  
  "---------------------------------------------------------------------
  " Add persistence-specific methods
  "---------------------------------------------------------------------
  METHODS:
    save RETURNING VALUE(rv_success) TYPE abap_bool,
    delete RETURNING VALUE(rv_success) TYPE abap_bool,
    refresh.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTING COMPOUND INTERFACE
*----------------------------------------------------------------------*
CLASS lcl_customer DEFINITION.
  PUBLIC SECTION.
    "-------------------------------------------------------------------
    " By implementing lif_business_entity, we must implement:
    " - All methods from lif_identifiable
    " - All methods from lif_timestamped
    " - All methods from lif_auditable
    " - All methods from lif_serializable
    " - All methods from lif_business_entity itself
    "-------------------------------------------------------------------
    INTERFACES: lif_business_entity.
    
    "-------------------------------------------------------------------
    " Create aliases for cleaner access
    "-------------------------------------------------------------------
    ALIASES:
      " From nested lif_identifiable
      get_id FOR lif_business_entity~lif_identifiable~get_id,
      
      " From nested lif_timestamped
      get_created_at FOR lif_business_entity~lif_timestamped~get_created_at,
      get_updated_at FOR lif_business_entity~lif_timestamped~get_updated_at,
      
      " From lif_business_entity itself
      validate FOR lif_business_entity~validate,
      get_display_name FOR lif_business_entity~get_display_name.
    
    METHODS: constructor
      IMPORTING
        iv_id   TYPE string
        iv_name TYPE string.

  PRIVATE SECTION.
    DATA:
      mv_id         TYPE string,
      mv_name       TYPE string,
      mv_created_at TYPE timestamp,
      mv_updated_at TYPE timestamp,
      mv_created_by TYPE string,
      mv_updated_by TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_customer IMPLEMENTATION.

  METHOD constructor.
    mv_id   = iv_id.
    mv_name = iv_name.
    mv_created_by = sy-uname.
    mv_updated_by = sy-uname.
    GET TIME STAMP FIELD mv_created_at.
    mv_updated_at = mv_created_at.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Implementation of nested interface lif_identifiable
  " Note the double ~ for nested interfaces
  "---------------------------------------------------------------------
  METHOD lif_business_entity~lif_identifiable~get_id.
    rv_id = mv_id.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Implementation of nested interface lif_timestamped
  "---------------------------------------------------------------------
  METHOD lif_business_entity~lif_timestamped~get_created_at.
    rv_timestamp = mv_created_at.
  ENDMETHOD.

  METHOD lif_business_entity~lif_timestamped~get_updated_at.
    rv_timestamp = mv_updated_at.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Implementation of nested interface lif_auditable
  "---------------------------------------------------------------------
  METHOD lif_business_entity~lif_auditable~get_created_by.
    rv_user = mv_created_by.
  ENDMETHOD.

  METHOD lif_business_entity~lif_auditable~get_updated_by.
    rv_user = mv_updated_by.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Implementation of nested interface lif_serializable
  "---------------------------------------------------------------------
  METHOD lif_business_entity~lif_serializable~to_json.
    rv_json = |\{ "id": "{ mv_id }", "name": "{ mv_name }" \}|.
  ENDMETHOD.

  METHOD lif_business_entity~lif_serializable~to_xml.
    rv_xml = |<customer><id>{ mv_id }</id><name>{ mv_name }</name></customer>|.
  ENDMETHOD.

  "---------------------------------------------------------------------
  " Implementation of lif_business_entity own methods
  "---------------------------------------------------------------------
  METHOD lif_business_entity~validate.
    IF mv_id IS NOT INITIAL AND mv_name IS NOT INITIAL.
      rv_valid = abap_true.
    ELSE.
      rv_valid = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD lif_business_entity~get_display_name.
    rv_name = |{ mv_name } ({ mv_id })|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_customer TYPE REF TO lcl_customer,
        lv_output   TYPE string.

  CREATE OBJECT lo_customer
    EXPORTING
      iv_id   = 'CUST001'
      iv_name = 'John Doe'.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'COMPOUND/NESTED INTERFACE DEMONSTRATION'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  SKIP.

  " Using aliases (short form)
  WRITE: / 'Customer ID:', lo_customer->get_id( ).
  WRITE: / 'Display Name:', lo_customer->get_display_name( ).
  WRITE: / 'Is Valid:', lo_customer->validate( ).
  SKIP.

  " Using full path (long form)
  lv_output = lo_customer->lif_business_entity~lif_serializable~to_json( ).
  WRITE: / 'JSON:', lv_output.
  
  lv_output = lo_customer->lif_business_entity~lif_serializable~to_xml( ).
  WRITE: / 'XML:', lv_output.
  SKIP.

  WRITE: / 'Created By:', 
         lo_customer->lif_business_entity~lif_auditable~get_created_by( ).
  WRITE: / 'Created At:', 
         lo_customer->lif_business_entity~lif_timestamped~get_created_at( ).
```

#### **Output:**

```
═══════════════════════════════════════════════════════
COMPOUND/NESTED INTERFACE DEMONSTRATION
═══════════════════════════════════════════════════════

Customer ID: CUST001
Display Name: John Doe (CUST001)
Is Valid: X

JSON: { "id": "CUST001", "name": "John Doe" }
XML: <customer><id>CUST001</id><name>John Doe</name></customer>

Created By: DEVELOPER
Created At: 20241215123456
```

---

### **10 Interface Reference Variables**

#### **Using Interface Types for Object References**

```
╔═══════════════════════════════════════════════════════════════╗
║             INTERFACE REFERENCE VARIABLES                     ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   You can declare variables of INTERFACE type                 ║
║   These can hold ANY object that implements the interface     ║
║                                                               ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │  DATA: lo_printable TYPE REF TO lif_printable.          │ ║
║   │                                                         │ ║
║   │  " Can hold any object implementing lif_printable:      │ ║
║   │  lo_printable = lo_report.   " Report object            │ ║
║   │  lo_printable = lo_invoice.  " Invoice object           │ ║
║   │  lo_printable = lo_receipt.  " Receipt object           │ ║
║   │                                                         │ ║
║   │  " All can be printed the same way:                     │ ║
║   │  lo_printable->print( ).                                │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   THIS IS POLYMORPHISM!                                       ║
║   Same variable, same method call, different behaviors        ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Complete Example: Polymorphism with Interface References**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_REFERENCE
*& Description: Using interface reference variables for polymorphism
*&---------------------------------------------------------------------*
REPORT z_interface_reference.

*----------------------------------------------------------------------*
* INTERFACE DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_shape.
  METHODS:
    get_area RETURNING VALUE(rv_area) TYPE f,
    get_perimeter RETURNING VALUE(rv_perimeter) TYPE f,
    get_name RETURNING VALUE(rv_name) TYPE string,
    draw.
ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: CIRCLE
*----------------------------------------------------------------------*
CLASS lcl_circle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_shape.
    ALIASES: get_area FOR lif_shape~get_area,
             draw     FOR lif_shape~draw.
    METHODS: constructor IMPORTING iv_radius TYPE f.
  PRIVATE SECTION.
    DATA: mv_radius TYPE f.
    CONSTANTS: cv_pi TYPE f VALUE '3.14159'.
ENDCLASS.

CLASS lcl_circle IMPLEMENTATION.
  METHOD constructor.
    mv_radius = iv_radius.
  ENDMETHOD.
  
  METHOD lif_shape~get_area.
    rv_area = cv_pi * mv_radius * mv_radius.
  ENDMETHOD.
  
  METHOD lif_shape~get_perimeter.
    rv_perimeter = 2 * cv_pi * mv_radius.
  ENDMETHOD.
  
  METHOD lif_shape~get_name.
    rv_name = 'Circle'.
  ENDMETHOD.
  
  METHOD lif_shape~draw.
    WRITE: / '    ****    '.
    WRITE: / '  *      *  '.
    WRITE: / ' *        * '.
    WRITE: / ' *        * '.
    WRITE: / '  *      *  '.
    WRITE: / '    ****    '.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: RECTANGLE
*----------------------------------------------------------------------*
CLASS lcl_rectangle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_shape.
    ALIASES: get_area FOR lif_shape~get_area,
             draw     FOR lif_shape~draw.
    METHODS: constructor 
      IMPORTING iv_length TYPE f
                iv_width  TYPE f.
  PRIVATE SECTION.
    DATA: mv_length TYPE f,
          mv_width  TYPE f.
ENDCLASS.

CLASS lcl_rectangle IMPLEMENTATION.
  METHOD constructor.
    mv_length = iv_length.
    mv_width  = iv_width.
  ENDMETHOD.
  
  METHOD lif_shape~get_area.
    rv_area = mv_length * mv_width.
  ENDMETHOD.
  
  METHOD lif_shape~get_perimeter.
    rv_perimeter = 2 * ( mv_length + mv_width ).
  ENDMETHOD.
  
  METHOD lif_shape~get_name.
    rv_name = 'Rectangle'.
  ENDMETHOD.
  
  METHOD lif_shape~draw.
    WRITE: / '************'.
    WRITE: / '*          *'.
    WRITE: / '*          *'.
    WRITE: / '************'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: TRIANGLE
*----------------------------------------------------------------------*
CLASS lcl_triangle DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_shape.
    ALIASES: get_area FOR lif_shape~get_area,
             draw     FOR lif_shape~draw.
    METHODS: constructor 
      IMPORTING iv_base   TYPE f
                iv_height TYPE f
                iv_side1  TYPE f
                iv_side2  TYPE f.
  PRIVATE SECTION.
    DATA: mv_base   TYPE f,
          mv_height TYPE f,
          mv_side1  TYPE f,
          mv_side2  TYPE f.
ENDCLASS.

CLASS lcl_triangle IMPLEMENTATION.
  METHOD constructor.
    mv_base   = iv_base.
    mv_height = iv_height.
    mv_side1  = iv_side1.
    mv_side2  = iv_side2.
  ENDMETHOD.
  
  METHOD lif_shape~get_area.
    rv_area = ( mv_base * mv_height ) / 2.
  ENDMETHOD.
  
  METHOD lif_shape~get_perimeter.
    rv_perimeter = mv_base + mv_side1 + mv_side2.
  ENDMETHOD.
  
  METHOD lif_shape~get_name.
    rv_name = 'Triangle'.
  ENDMETHOD.
  
  METHOD lif_shape~draw.
    WRITE: / '     *     '.
    WRITE: / '    * *    '.
    WRITE: / '   *   *   '.
    WRITE: / '  *     *  '.
    WRITE: / ' *       * '.
    WRITE: / '***********'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* HELPER CLASS: SHAPE CALCULATOR
* Uses interface reference to work with ANY shape!
*----------------------------------------------------------------------*
CLASS lcl_shape_calculator DEFINITION.
  PUBLIC SECTION.
    "-------------------------------------------------------------------
    " Method accepts INTERFACE reference, not specific class
    " This means it can work with Circle, Rectangle, Triangle, etc.
    "-------------------------------------------------------------------
    CLASS-METHODS:
      print_shape_info
        IMPORTING io_shape TYPE REF TO lif_shape,
      
      compare_areas
        IMPORTING io_shape1 TYPE REF TO lif_shape
                  io_shape2 TYPE REF TO lif_shape,
      
      process_shapes
        IMPORTING it_shapes TYPE TABLE.
ENDCLASS.

CLASS lcl_shape_calculator IMPLEMENTATION.

  METHOD print_shape_info.
    WRITE: / '╔═══════════════════════════════════════╗'.
    WRITE: / '║ Shape:', io_shape->lif_shape~get_name( ).
    WRITE: / '╠═══════════════════════════════════════╣'.
    WRITE: / '║ Area:', io_shape->lif_shape~get_area( ).
    WRITE: / '║ Perimeter:', io_shape->lif_shape~get_perimeter( ).
    WRITE: / '╠═══════════════════════════════════════╣'.
    io_shape->lif_shape~draw( ).
    WRITE: / '╚═══════════════════════════════════════╝'.
    SKIP.
  ENDMETHOD.

  METHOD compare_areas.
    DATA: lv_area1 TYPE f,
          lv_area2 TYPE f.
    
    lv_area1 = io_shape1->lif_shape~get_area( ).
    lv_area2 = io_shape2->lif_shape~get_area( ).
    
    WRITE: / 'Comparing:', io_shape1->lif_shape~get_name( ), 
             'vs', io_shape2->lif_shape~get_name( ).
    
    IF lv_area1 > lv_area2.
      WRITE: / io_shape1->lif_shape~get_name( ), 'is larger'.
    ELSEIF lv_area2 > lv_area1.
      WRITE: / io_shape2->lif_shape~get_name( ), 'is larger'.
    ELSE.
      WRITE: / 'Both have equal area'.
    ENDIF.
    SKIP.
  ENDMETHOD.

  METHOD process_shapes.
    DATA: lo_shape TYPE REF TO lif_shape,
          lv_total_area TYPE f.
    
    WRITE: / '═══════════════════════════════════════'.
    WRITE: / 'PROCESSING ALL SHAPES IN COLLECTION'.
    WRITE: / '═══════════════════════════════════════'.
    
    LOOP AT it_shapes INTO lo_shape.
      lv_total_area = lv_total_area + lo_shape->lif_shape~get_area( ).
      WRITE: / '-', lo_shape->lif_shape~get_name( ), 
               'Area:', lo_shape->lif_shape~get_area( ).
    ENDLOOP.
    
    SKIP.
    WRITE: / 'TOTAL AREA:', lv_total_area.
    SKIP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Concrete class references
  DATA: lo_circle    TYPE REF TO lcl_circle,
        lo_rectangle TYPE REF TO lcl_rectangle,
        lo_triangle  TYPE REF TO lcl_triangle.

  " INTERFACE reference - can hold ANY shape!
  DATA: lo_shape     TYPE REF TO lif_shape.

  " Table of interface references
  DATA: lt_shapes TYPE TABLE OF REF TO lif_shape.

  " Create shapes
  CREATE OBJECT lo_circle EXPORTING iv_radius = '5.0'.
  CREATE OBJECT lo_rectangle 
    EXPORTING iv_length = '10.0' 
              iv_width  = '5.0'.
  CREATE OBJECT lo_triangle
    EXPORTING iv_base   = '6.0'
              iv_height = '4.0'
              iv_side1  = '5.0'
              iv_side2  = '5.0'.

  "---------------------------------------------------------------------
  " POLYMORPHISM: Same interface variable, different implementations
  "---------------------------------------------------------------------
  WRITE: / '═══════════════════════════════════════'.
  WRITE: / 'POLYMORPHISM WITH INTERFACE REFERENCE'.
  WRITE: / '═══════════════════════════════════════'.
  SKIP.

  " Assign circle to interface reference
  lo_shape = lo_circle.
  WRITE: / 'Shape is:', lo_shape->lif_shape~get_name( ).
  WRITE: / 'Area:', lo_shape->lif_shape~get_area( ).
  SKIP.

  " Assign rectangle to SAME interface reference
  lo_shape = lo_rectangle.
  WRITE: / 'Shape is:', lo_shape->lif_shape~get_name( ).
  WRITE: / 'Area:', lo_shape->lif_shape~get_area( ).
  SKIP.

  " Assign triangle to SAME interface reference
  lo_shape = lo_triangle.
  WRITE: / 'Shape is:', lo_shape->lif_shape~get_name( ).
  WRITE: / 'Area:', lo_shape->lif_shape~get_area( ).
  SKIP.

  "---------------------------------------------------------------------
  " Using interface reference with helper class
  "---------------------------------------------------------------------
  lcl_shape_calculator=>print_shape_info( lo_circle ).
  lcl_shape_calculator=>print_shape_info( lo_rectangle ).

  "---------------------------------------------------------------------
  " Compare shapes
  "---------------------------------------------------------------------
  lcl_shape_calculator=>compare_areas(
    io_shape1 = lo_circle
    io_shape2 = lo_rectangle ).

  "---------------------------------------------------------------------
  " Process collection of shapes
  "---------------------------------------------------------------------
  APPEND lo_circle TO lt_shapes.
  APPEND lo_rectangle TO lt_shapes.
  APPEND lo_triangle TO lt_shapes.

  lcl_shape_calculator=>process_shapes( lt_shapes ).
```

#### **Output:**

```
═══════════════════════════════════════
POLYMORPHISM WITH INTERFACE REFERENCE
═══════════════════════════════════════

Shape is: Circle
Area: 78.53975

Shape is: Rectangle
Area: 50.00000

Shape is: Triangle
Area: 12.00000

╔═══════════════════════════════════════╗
║ Shape: Circle
╠═══════════════════════════════════════╣
║ Area: 78.53975
║ Perimeter: 31.4159
╠═══════════════════════════════════════╣
    ****    
  *      *  
 *        * 
 *        * 
  *      *  
    ****    
╚═══════════════════════════════════════╝

╔═══════════════════════════════════════╗
║ Shape: Rectangle
╠═══════════════════════════════════════╣
║ Area: 50.00000
║ Perimeter: 30.00000
╠═══════════════════════════════════════╣
************
*          *
*          *
************
╚═══════════════════════════════════════╝

Comparing: Circle vs Rectangle
Circle is larger

═══════════════════════════════════════
PROCESSING ALL SHAPES IN COLLECTION
═══════════════════════════════════════
- Circle Area: 78.53975
- Rectangle Area: 50.00000
- Triangle Area: 12.00000

TOTAL AREA: 140.53975
```

---

### **11 Interface Constants and Types**

#### **Detailed Example of Interface Constants and Types**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_CONSTANTS_TYPES
*& Description: Using interface constants and types effectively
*&---------------------------------------------------------------------*
REPORT z_interface_constants_types.

*----------------------------------------------------------------------*
* INTERFACE WITH COMPREHENSIVE CONSTANTS AND TYPES
*----------------------------------------------------------------------*
INTERFACE lif_order_status.
  
  "=====================================================================
  " TYPE DEFINITIONS
  " Purpose: Create reusable types for implementing classes
  "=====================================================================
  TYPES:
    " Enum-like type for order status
    ty_status TYPE c LENGTH 2,
    
    " Status description
    ty_status_text TYPE c LENGTH 30,
    
    " Order priority type
    ty_priority TYPE c LENGTH 1,
    
    " Order item structure
    BEGIN OF ty_order_item,
      item_no     TYPE numc10,
      material_no TYPE matnr,
      quantity    TYPE menge_d,
      price       TYPE netpr_ap,
    END OF ty_order_item,
    
    " Order items table
    ty_t_order_items TYPE STANDARD TABLE OF ty_order_item WITH DEFAULT KEY,
    
    " Order header structure
    BEGIN OF ty_order_header,
      order_no    TYPE vbeln,
      customer_no TYPE kunnr,
      order_date  TYPE datum,
      status      TYPE ty_status,
      priority    TYPE ty_priority,
    END OF ty_order_header.

  "=====================================================================
  " CONSTANTS
  " Purpose: Provide standard values that can be used by all 
  "          implementing classes
  "=====================================================================
  CONSTANTS:
    " Status values (like enum)
    cv_status_new        TYPE ty_status VALUE '01',
    cv_status_processing TYPE ty_status VALUE '02',
    cv_status_shipped    TYPE ty_status VALUE '03',
    cv_status_delivered  TYPE ty_status VALUE '04',
    cv_status_cancelled  TYPE ty_status VALUE '99',
    
    " Priority values
    cv_priority_low      TYPE ty_priority VALUE 'L',
    cv_priority_medium   TYPE ty_priority VALUE 'M',
    cv_priority_high     TYPE ty_priority VALUE 'H',
    cv_priority_critical TYPE ty_priority VALUE 'C',
    
    " Business rules
    cv_max_items_per_order TYPE i VALUE 100,
    cv_min_order_amount    TYPE p DECIMALS 2 VALUE '10.00',
    cv_max_order_amount    TYPE p DECIMALS 2 VALUE '1000000.00',
    
    " Messages
    cv_msg_order_created   TYPE string VALUE 'Order created successfully',
    cv_msg_order_updated   TYPE string VALUE 'Order updated successfully',
    cv_msg_order_cancelled TYPE string VALUE 'Order has been cancelled'.

  "=====================================================================
  " METHODS using the defined types and constants
  "=====================================================================
  METHODS:
    create_order
      IMPORTING is_header TYPE ty_order_header
                it_items  TYPE ty_t_order_items
      RETURNING VALUE(rv_order_no) TYPE vbeln,
    
    get_status_text
      IMPORTING iv_status TYPE ty_status
      RETURNING VALUE(rv_text) TYPE ty_status_text,
    
    update_status
      IMPORTING iv_order_no   TYPE vbeln
                iv_new_status TYPE ty_status
      RETURNING VALUE(rv_success) TYPE abap_bool.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS IMPLEMENTING THE INTERFACE
*----------------------------------------------------------------------*
CLASS lcl_order_manager DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_order_status.
    
    ALIASES:
      create_order    FOR lif_order_status~create_order,
      get_status_text FOR lif_order_status~get_status_text,
      update_status   FOR lif_order_status~update_status.
    
    METHODS: constructor.

  PRIVATE SECTION.
    DATA: mt_orders TYPE TABLE OF lif_order_status=>ty_order_header,
          mv_next_order_no TYPE numc10.
ENDCLASS.

CLASS lcl_order_manager IMPLEMENTATION.

  METHOD constructor.
    mv_next_order_no = '1000000001'.
  ENDMETHOD.

  METHOD lif_order_status~create_order.
    DATA: ls_order TYPE lif_order_status=>ty_order_header.
    
    " Validate against constants
    IF lines( it_items ) > lif_order_status=>cv_max_items_per_order.
      WRITE: / 'Error: Too many items. Max allowed:', 
               lif_order_status=>cv_max_items_per_order.
      RETURN.
    ENDIF.
    
    " Create order using interface types
    ls_order = is_header.
    ls_order-order_no = mv_next_order_no.
    ls_order-status = lif_order_status=>cv_status_new.
    
    APPEND ls_order TO mt_orders.
    rv_order_no = ls_order-order_no.
    
    mv_next_order_no = mv_next_order_no + 1.
    
    WRITE: / lif_order_status=>cv_msg_order_created.
    WRITE: / 'Order Number:', rv_order_no.
  ENDMETHOD.

  METHOD lif_order_status~get_status_text.
    " Using constants for comparison
    CASE iv_status.
      WHEN lif_order_status=>cv_status_new.
        rv_text = 'New Order'.
      WHEN lif_order_status=>cv_status_processing.
        rv_text = 'In Processing'.
      WHEN lif_order_status=>cv_status_shipped.
        rv_text = 'Shipped'.
      WHEN lif_order_status=>cv_status_delivered.
        rv_text = 'Delivered'.
      WHEN lif_order_status=>cv_status_cancelled.
        rv_text = 'Cancelled'.
      WHEN OTHERS.
        rv_text = 'Unknown Status'.
    ENDCASE.
  ENDMETHOD.

  METHOD lif_order_status~update_status.
    DATA: ls_order TYPE lif_order_status=>ty_order_header.
    
    READ TABLE mt_orders INTO ls_order 
         WITH KEY order_no = iv_order_no.
    
    IF sy-subrc = 0.
      ls_order-status = iv_new_status.
      MODIFY TABLE mt_orders FROM ls_order.
      rv_success = abap_true.
      WRITE: / lif_order_status=>cv_msg_order_updated.
    ELSE.
      rv_success = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_order_mgr TYPE REF TO lcl_order_manager,
        ls_header    TYPE lif_order_status=>ty_order_header,
        lt_items     TYPE lif_order_status=>ty_t_order_items,
        ls_item      TYPE lif_order_status=>ty_order_item,
        lv_order_no  TYPE vbeln,
        lv_status_text TYPE lif_order_status=>ty_status_text.

  CREATE OBJECT lo_order_mgr.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'INTERFACE CONSTANTS AND TYPES DEMONSTRATION'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  SKIP.

  " Display constants from interface
  WRITE: / 'INTERFACE CONSTANTS:'.
  WRITE: / '-------------------'.
  WRITE: / 'Status NEW:', lif_order_status=>cv_status_new.
  WRITE: / 'Status PROCESSING:', lif_order_status=>cv_status_processing.
  WRITE: / 'Status SHIPPED:', lif_order_status=>cv_status_shipped.
  WRITE: / 'Max items per order:', lif_order_status=>cv_max_items_per_order.
  WRITE: / 'Min order amount:', lif_order_status=>cv_min_order_amount.
  SKIP.

  " Create order using interface types
  ls_header-customer_no = 'CUST001'.
  ls_header-order_date  = sy-datum.
  ls_header-priority    = lif_order_status=>cv_priority_high.

  ls_item-item_no     = '0001'.
  ls_item-material_no = 'MAT001'.
  ls_item-quantity    = 10.
  ls_item-price       = '25.00'.
  APPEND ls_item TO lt_items.

  ls_item-item_no     = '0002'.
  ls_item-material_no = 'MAT002'.
  ls_item-quantity    = 5.
  ls_item-price       = '50.00'.
  APPEND ls_item TO lt_items.

  WRITE: / 'CREATING ORDER:'.
  WRITE: / '---------------'.
  lv_order_no = lo_order_mgr->create_order(
    is_header = ls_header
    it_items  = lt_items ).
  SKIP.

  " Get status text
  lv_status_text = lo_order_mgr->get_status_text( 
    lif_order_status=>cv_status_new ).
  WRITE: / 'Status Text:', lv_status_text.
  SKIP.

  " Update status
  WRITE: / 'UPDATING STATUS:'.
  WRITE: / '----------------'.
  lo_order_mgr->update_status(
    iv_order_no   = lv_order_no
    iv_new_status = lif_order_status=>cv_status_processing ).
  
  lv_status_text = lo_order_mgr->get_status_text(
    lif_order_status=>cv_status_processing ).
  WRITE: / 'New Status Text:', lv_status_text.
```

#### **Output:**

```
═══════════════════════════════════════════════════════
INTERFACE CONSTANTS AND TYPES DEMONSTRATION
═══════════════════════════════════════════════════════

INTERFACE CONSTANTS:
-------------------
Status NEW: 01
Status PROCESSING: 02
Status SHIPPED: 03
Max items per order: 100
Min order amount: 10.00

CREATING ORDER:
---------------
Order created successfully
Order Number: 1000000001

Status Text: New Order

UPDATING STATUS:
----------------
Order updated successfully
New Status Text: In Processing
```

---

### **12 Events in Interfaces**

#### **Interface Events Explained**

```
╔═══════════════════════════════════════════════════════════════╗
║                 EVENTS IN INTERFACES                          ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   Interfaces can declare EVENTS                               ║
║   Implementing classes can RAISE these events                 ║
║   Other classes can HANDLE (listen to) these events           ║
║                                                               ║
║   ┌────────────────────────────────────────────────────────┐  ║
║   │  INTERFACE lif_observable                              │  ║
║   │    EVENTS: data_changed                                │  ║
║   │            EXPORTING VALUE(ev_old) ev_new              │  ║
║   └────────────────────────────────────────────────────────┘  ║
║                          │                                    ║
║                          ▼                                    ║
║   ┌────────────────────────────────────────────────────────┐  ║
║   │  CLASS lcl_data_source                                 │  ║
║   │    INTERFACES: lif_observable                          │  ║
║   │    ...                                                 │  ║
║   │    RAISE EVENT lif_observable~data_changed             │  ║
║   └────────────────────────────────────────────────────────┘  ║
║                          │                                    ║
║                          ▼ Event raised                       ║
║   ┌────────────────────────────────────────────────────────┐  ║
║   │  CLASS lcl_handler                                     │  ║
║   │    SET HANDLER me->on_data_changed                     │  ║
║   │              FOR lo_data_source                        │  ║
║   │    ...                                                 │  ║
║   │  METHOD on_data_changed. " Handles the event          │  ║
║   └────────────────────────────────────────────────────────┘  ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Complete Event Example**

```abap
*&---------------------------------------------------------------------*
*& Report: Z_INTERFACE_EVENTS
*& Description: Events declared in interfaces and handled by classes
*&---------------------------------------------------------------------*
REPORT z_interface_events.

*----------------------------------------------------------------------*
* INTERFACE WITH EVENTS
*----------------------------------------------------------------------*
INTERFACE lif_stock_monitor.
  
  "=====================================================================
  " EVENT DECLARATIONS
  "=====================================================================
  EVENTS:
    " Event when stock level changes
    stock_changed
      EXPORTING
        VALUE(ev_material) TYPE matnr
        VALUE(ev_old_qty)  TYPE menge_d
        VALUE(ev_new_qty)  TYPE menge_d,
    
    " Event when stock falls below minimum
    low_stock_alert
      EXPORTING
        VALUE(ev_material) TYPE matnr
        VALUE(ev_current)  TYPE menge_d
        VALUE(ev_minimum)  TYPE menge_d,
    
    " Event when stock is replenished
    stock_replenished
      EXPORTING
        VALUE(ev_material) TYPE matnr
        VALUE(ev_quantity) TYPE menge_d.

  "=====================================================================
  " METHODS
  "=====================================================================
  METHODS:
    update_stock
      IMPORTING iv_material TYPE matnr
                iv_quantity TYPE menge_d,
    
    get_stock_level
      IMPORTING iv_material TYPE matnr
      RETURNING VALUE(rv_quantity) TYPE menge_d.

ENDINTERFACE.

*----------------------------------------------------------------------*
* CLASS: WAREHOUSE (Implements interface and raises events)
*----------------------------------------------------------------------*
CLASS lcl_warehouse DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_stock_monitor.
    
    ALIASES:
      update_stock    FOR lif_stock_monitor~update_stock,
      get_stock_level FOR lif_stock_monitor~get_stock_level.
    
    METHODS: constructor
      IMPORTING iv_warehouse_id TYPE string.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_stock,
        material TYPE matnr,
        quantity TYPE menge_d,
        minimum  TYPE menge_d,
      END OF ty_stock.
    
    DATA: mv_warehouse_id TYPE string,
          mt_stock        TYPE HASHED TABLE OF ty_stock 
                          WITH UNIQUE KEY material.
    
    CONSTANTS: cv_default_minimum TYPE menge_d VALUE 50.
ENDCLASS.

CLASS lcl_warehouse IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_stock TYPE ty_stock.
    
    mv_warehouse_id = iv_warehouse_id.
    
    " Initialize some stock
    ls_stock-material = 'MAT001'.
    ls_stock-quantity = 100.
    ls_stock-minimum  = cv_default_minimum.
    INSERT ls_stock INTO TABLE mt_stock.
    
    ls_stock-material = 'MAT002'.
    ls_stock-quantity = 75.
    ls_stock-minimum  = cv_default_minimum.
    INSERT ls_stock INTO TABLE mt_stock.
    
    ls_stock-material = 'MAT003'.
    ls_stock-quantity = 30.
    ls_stock-minimum  = cv_default_minimum.
    INSERT ls_stock INTO TABLE mt_stock.
  ENDMETHOD.

  METHOD lif_stock_monitor~update_stock.
    DATA: ls_stock   TYPE ty_stock,
          lv_old_qty TYPE menge_d.

    READ TABLE mt_stock INTO ls_stock WITH KEY material = iv_material.
    
    IF sy-subrc = 0.
      lv_old_qty = ls_stock-quantity.
      ls_stock-quantity = iv_quantity.
      MODIFY TABLE mt_stock FROM ls_stock.
      
      "-------------------------------------------------------------
      " RAISE stock_changed event
      " This will notify all registered handlers
      "-------------------------------------------------------------
      RAISE EVENT lif_stock_monitor~stock_changed
        EXPORTING
          ev_material = iv_material
          ev_old_qty  = lv_old_qty
          ev_new_qty  = iv_quantity.
      
      "-------------------------------------------------------------
      " Check if stock is low and raise alert
      "-------------------------------------------------------------
      IF iv_quantity < ls_stock-minimum.
        RAISE EVENT lif_stock_monitor~low_stock_alert
          EXPORTING
            ev_material = iv_material
            ev_current  = iv_quantity
            ev_minimum  = ls_stock-minimum.
      ENDIF.
      
      "-------------------------------------------------------------
      " Check if stock was replenished
      "-------------------------------------------------------------
      IF iv_quantity > lv_old_qty AND lv_old_qty < ls_stock-minimum.
        RAISE EVENT lif_stock_monitor~stock_replenished
          EXPORTING
            ev_material = iv_material
            ev_quantity = iv_quantity - lv_old_qty.
      ENDIF.
      
    ELSE.
      WRITE: / 'Material not found:', iv_material.
    ENDIF.
  ENDMETHOD.

  METHOD lif_stock_monitor~get_stock_level.
    DATA: ls_stock TYPE ty_stock.
    
    READ TABLE mt_stock INTO ls_stock WITH KEY material = iv_material.
    IF sy-subrc = 0.
      rv_quantity = ls_stock-quantity.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: STOCK ALERT HANDLER (Handles events)
*----------------------------------------------------------------------*
CLASS lcl_stock_alert_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor,
      
      " Event handler methods - must have matching signature
      on_stock_changed
        FOR EVENT stock_changed OF lif_stock_monitor
        IMPORTING ev_material ev_old_qty ev_new_qty,
      
      on_low_stock
        FOR EVENT low_stock_alert OF lif_stock_monitor
        IMPORTING ev_material ev_current ev_minimum,
      
      on_stock_replenished
        FOR EVENT stock_replenished OF lif_stock_monitor
        IMPORTING ev_material ev_quantity,
      
      register_for_events
        IMPORTING io_stock_source TYPE REF TO lif_stock_monitor.

  PRIVATE SECTION.
    DATA: mv_handler_name TYPE string.
ENDCLASS.

CLASS lcl_stock_alert_handler IMPLEMENTATION.

  METHOD constructor.
    mv_handler_name = 'Stock Alert Handler'.
  ENDMETHOD.

  METHOD register_for_events.
    "-------------------------------------------------------------------
    " SET HANDLER registers this object's methods to handle events
    " FOR io_stock_source - specifies which object's events to handle
    "-------------------------------------------------------------------
    SET HANDLER:
      me->on_stock_changed FOR io_stock_source,
      me->on_low_stock FOR io_stock_source,
      me->on_stock_replenished FOR io_stock_source.
    
    WRITE: / 'Event handlers registered for stock monitoring'.
  ENDMETHOD.

  METHOD on_stock_changed.
    WRITE: / '╔══════════════════════════════════════════════════════╗'.
    WRITE: / '║ EVENT: Stock Changed                                 ║'.
    WRITE: / '╠══════════════════════════════════════════════════════╣'.
    WRITE: / '║ Material:', ev_material.
    WRITE: / '║ Old Quantity:', ev_old_qty.
    WRITE: / '║ New Quantity:', ev_new_qty.
    WRITE: / '║ Change:', ev_new_qty - ev_old_qty.
    WRITE: / '╚══════════════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD on_low_stock.
    WRITE: / '╔══════════════════════════════════════════════════════╗'.
    WRITE: / '║ ⚠️  ALERT: Low Stock Warning!                         ║'.
    WRITE: / '╠══════════════════════════════════════════════════════╣'.
    WRITE: / '║ Material:', ev_material.
    WRITE: / '║ Current Stock:', ev_current.
    WRITE: / '║ Minimum Required:', ev_minimum.
    WRITE: / '║ ACTION REQUIRED: Please reorder!                     ║'.
    WRITE: / '╚══════════════════════════════════════════════════════╝'.
  ENDMETHOD.

  METHOD on_stock_replenished.
    WRITE: / '╔══════════════════════════════════════════════════════╗'.
    WRITE: / '║ ✓ Stock Replenished                                  ║'.
    WRITE: / '╠══════════════════════════════════════════════════════╣'.
    WRITE: / '║ Material:', ev_material.
    WRITE: / '║ Added Quantity:', ev_quantity.
    WRITE: / '╚══════════════════════════════════════════════════════╝'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CLASS: EMAIL NOTIFIER (Another handler for same events)
*----------------------------------------------------------------------*
CLASS lcl_email_notifier DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_low_stock
        FOR EVENT low_stock_alert OF lif_stock_monitor
        IMPORTING ev_material ev_current ev_minimum,
      
      register_for_low_stock
        IMPORTING io_stock_source TYPE REF TO lif_stock_monitor.
ENDCLASS.

CLASS lcl_email_notifier IMPLEMENTATION.

  METHOD register_for_low_stock.
    " Only register for low stock events
    SET HANDLER me->on_low_stock FOR io_stock_source.
    WRITE: / 'Email notifier registered for low stock alerts'.
  ENDMETHOD.

  METHOD on_low_stock.
    WRITE: / '📧 EMAIL SENT: Low stock alert for', ev_material,
           '- Current:', ev_current, 'Min:', ev_minimum.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lo_warehouse TYPE REF TO lcl_warehouse,
        lo_handler   TYPE REF TO lcl_stock_alert_handler,
        lo_email     TYPE REF TO lcl_email_notifier,
        lo_stock_if  TYPE REF TO lif_stock_monitor.

  " Create objects
  CREATE OBJECT lo_warehouse EXPORTING iv_warehouse_id = 'WH-001'.
  CREATE OBJECT lo_handler.
  CREATE OBJECT lo_email.

  " Get interface reference
  lo_stock_if = lo_warehouse.

  WRITE: / '═══════════════════════════════════════════════════════'.
  WRITE: / 'INTERFACE EVENTS DEMONSTRATION'.
  WRITE: / '═══════════════════════════════════════════════════════'.
  SKIP.

  " Register event handlers
  WRITE: / 'REGISTERING EVENT HANDLERS:'.
  WRITE: / '---------------------------'.
  lo_handler->register_for_events( lo_stock_if ).
  lo_email->register_for_low_stock( lo_stock_if ).
  SKIP.

  " Now update stock - this will raise events
  WRITE: / 'UPDATING STOCK (Events will be triggered):'.
  WRITE: / '-------------------------------------------'.
  SKIP.

  " Normal update
  lo_warehouse->update_stock( iv_material = 'MAT001'
                              iv_quantity = 80 ).
  SKIP.

  " Update that triggers low stock alert
  lo_warehouse->update_stock( iv_material = 'MAT002'
                              iv_quantity = 25 ).
  SKIP.

  " Replenishment
  lo_warehouse->update_stock( iv_material = 'MAT003'
                              iv_quantity = 100 ).
```

#### **Output:**

```
═══════════════════════════════════════════════════════
INTERFACE EVENTS DEMONSTRATION
═══════════════════════════════════════════════════════

REGISTERING EVENT HANDLERS:
---------------------------
Event handlers registered for stock monitoring
Email notifier registered for low stock alerts

UPDATING STOCK (Events will be triggered):
-------------------------------------------

╔══════════════════════════════════════════════════════╗
║ EVENT: Stock Changed                                 ║
╠══════════════════════════════════════════════════════╣
║ Material: MAT001
║ Old Quantity: 100
║ New Quantity: 80
║ Change: -20
╚══════════════════════════════════════════════════════╝

╔══════════════════════════════════════════════════════╗
║ EVENT: Stock Changed                                 ║
╠══════════════════════════════════════════════════════╣
║ Material: MAT002
║ Old Quantity: 75
║ New Quantity: 25
║ Change: -50
╚══════════════════════════════════════════════════════╝
╔══════════════════════════════════════════════════════╗
║ ⚠️  ALERT: Low Stock Warning!                         ║
╠══════════════════════════════════════════════════════╣
║ Material: MAT002
║ Current Stock: 25
║ Minimum Required: 50
║ ACTION REQUIRED: Please reorder!                     ║
╚══════════════════════════════════════════════════════╝
📧 EMAIL SENT: Low stock alert for MAT002 - Current: 25 Min: 50

╔══════════════════════════════════════════════════════╗
║ EVENT: Stock Changed                                 ║
╠══════════════════════════════════════════════════════╣
║ Material: MAT003
║ Old Quantity: 30
║ New Quantity: 100
║ Change: 70
╚══════════════════════════════════════════════════════╝
╔══════════════════════════════════════════════════════╗
║ ✓ Stock Replenished                                  ║
╠══════════════════════════════════════════════════════╣
║ Material: MAT003
║ Added Quantity: 70
╚══════════════════════════════════════════════════════╝
```

---

### **13 Industry Best Practices**

#### **Interface Naming Conventions**

```
╔═══════════════════════════════════════════════════════════════╗
║              INTERFACE NAMING CONVENTIONS                     ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   STANDARD SAP NAMING:                                        ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │                                                         │ ║
║   │   Global Interfaces: ZIF_<name> or YIF_<name>           │ ║
║   │   Examples:                                             │ ║
║   │   - ZIF_PRINTABLE                                       │ ║
║   │   - ZIF_ORDER_PROCESSOR                                 │ ║
║   │   - ZIF_PAYMENT_GATEWAY                                 │ ║
║   │                                                         │ ║
║   │   Local Interfaces: LIF_<name>                          │ ║
║   │   Examples:                                             │ ║
║   │   - LIF_CALCULATOR                                      │ ║
║   │   - LIF_VALIDATOR                                       │ ║
║   │                                                         │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   DESCRIPTIVE NAMING:                                         ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │                                                         │ ║
║   │   ✓ Use adjectives or "able" suffix for capabilities    │ ║
║   │     - IF_PRINTABLE, IF_SAVEABLE, IF_CACHEABLE           │ ║
║   │                                                         │ ║
║   │   ✓ Use nouns for service/handler interfaces            │ ║
║   │     - IF_PAYMENT_SERVICE, IF_ORDER_HANDLER              │ ║
║   │                                                         │ ║
║   │   ✗ Avoid generic names                                 │ ║
║   │     - IF_INTERFACE (too generic)                        │ ║
║   │     - IF_DATA (meaningless)                             │ ║
║   │                                                         │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

#### **Interface Design Principles**

```
╔═══════════════════════════════════════════════════════════════╗
║              INTERFACE DESIGN PRINCIPLES                      ║
╠═══════════════════════════════════════════════════════════════╣
║                                                               ║
║   1. INTERFACE SEGREGATION PRINCIPLE (ISP)                    ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │                                                         │ ║
║   │   ✗ BAD: One large interface with many methods          │ ║
║   │   ┌─────────────────────────────────────────────┐       │ ║
║   │   │  IF_DOCUMENT                                │       │ ║
║   │   │  - print()                                  │       │ ║
║   │   │  - save()                                   │       │ ║
║   │   │  - email()                                  │       │ ║
║   │   │  - fax()                                    │       │ ║
║   │   │  - archive()                                │       │ ║
║   │   │  - encrypt()                                │       │ ║
║   │   │  - compress()                               │       │ ║
║   │   └─────────────────────────────────────────────┘       │ ║
║   │   Problem: Class must implement ALL even if not needed  │ ║
║   │                                                         │ ║
║   │   ✓ GOOD: Small, focused interfaces                     │ ║
║   │   ┌──────────────┐  ┌──────────────┐                    │ ║
║   │   │IF_PRINTABLE  │  │IF_SAVEABLE   │                    │ ║
║   │   │- print()     │  │- save()      │                    │ ║
║   │   └──────────────┘  └──────────────┘                    │ ║
║   │   ┌──────────────┐  ┌──────────────┐                    │ ║
║   │   │IF_MAILABLE   │  │IF_ARCHIVABLE │                    │ ║
║   │   │- email()     │  │- archive()   │                    │ ║
║   │   └──────────────┘  └──────────────┘                    │ ║
║   │   Class only implements what it needs!                  │ ║
║   │                                                         │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   2. PROGRAM TO INTERFACE, NOT IMPLEMENTATION                 ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │                                                         │ ║
║   │   ✗ BAD:                                                │ ║
║   │   DATA: lo_calc TYPE REF TO lcl_simple_calculator.      │ ║
║   │                                                         │ ║
║   │   ✓ GOOD:                                               │ ║
║   │   DATA: lo_calc TYPE REF TO lif_calculator.             │ ║
║   │                                                         │ ║
║   │   Benefit: Can swap implementation without code change  │ ║
║   │                                                         │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
║   3. USE INTERFACES FOR DEPENDENCY INJECTION                  ║
║   ┌─────────────────────────────────────────────────────────┐ ║
║   │                                                         │ ║
║   │   METHODS: constructor                                  │ ║
║   │     IMPORTING io_logger TYPE REF TO lif_logger.         │ ║
║   │                                                         │ ║
║   │   " Can inject any logger implementation:               │ ║
║   │   " - lcl_file_logger                                   │ ║
║   │   " - lcl_db_logger                                     │ ║
║   │   " - lcl_mock_logger (for testing)                     │ ║
║   │                                                         │ ║
║   └─────────────────────────────────────────────────────────┘ ║
║                                                               ║
╚═══════════════════════════════════════════════════════════════╝
```

---
