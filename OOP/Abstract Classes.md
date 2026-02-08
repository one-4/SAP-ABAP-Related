

##  **ABSTRACT CLASSES**

---

## **TABLE OF CONTENTS**

1. [What Is an Abstract Class?](#1-what-is-an-abstract-class)
2. [Syntax of Abstract Classes](#2-syntax-of-abstract-classes)
3. [First Complete Example — Payment Processing System](#3-first-complete-example--payment-processing-system)
4. [Abstract Class vs Regular Class — Detailed Comparison](#4-abstract-class-vs-regular-class--detailed-comparison)
5. [What Happens If You Try to Instantiate an Abstract Class?](#5-what-happens-if-you-try-to-instantiate-an-abstract-class)
6. [What Happens If Subclass Doesn't Implement Abstract Methods?](#6-what-happens-if-subclass-doesnt-implement-all-abstract-methods)
7. [Multi-Level Abstract Inheritance](#7-multi-level-abstract-inheritance)
8. [Abstract Classes with Static Methods](#8-abstract-classes-with-static-methods)
9. [Exercise 1: Employee Salary System](#9-exercise-1-employee-salary-system)
10. [Industry Best Practices for Abstract Classes](#10-industry-best-practices-for-abstract-classes)
11. [When to Use Abstract Class vs Regular Class vs Final Class](#11-when-to-use-abstract-class-vs-regular-class-vs-final-class)
12. [Chapter Summary](#12-chapter-summary)

---

### **1 What Is an Abstract Class?**

#### **Real-World Analogy: The Master Architect**
Imagine you are the head architect of a large construction company. You create a master blueprint that states:

- **Every building MUST have a foundation**
- **Every building MUST have walls**  
- **Every building MUST have a roof**

However, you do **NOT** specify **HOW** to build these components—because a hospital's foundation differs from a shopping mall's foundation. You leave those implementation details to the specific building engineers.

**That master blueprint is an Abstract Class.**

#### **Definition in Simple Terms**
An **Abstract Class** is a class that **cannot be instantiated directly** (you cannot create an object from it). It serves as a **template or blueprint** for other classes. It can contain:

1. **Abstract methods** — Methods that have **NO implementation** (no code body). Subclasses **MUST** provide the implementation.  
   *Link: See [Syntax of Abstract Classes](#2-syntax-of-abstract-classes) for code examples*

2. **Concrete methods** — Methods that have **FULL implementation** (complete code body). Subclasses inherit them as-is or can override them.  
   *Link: See [First Complete Example](#3-first-complete-example--payment-processing-system) to understand how concrete methods work in abstract classes*

#### **Why Do We Need Abstract Classes?**

Abstract classes solve several real-world problems that arise in large codebases:

| Problem Without Abstract Classes | Solution With Abstract Classes |
|-----------------------------------|--------------------------------|
| Every developer writes payment logic differently | Abstract class enforces a standard structure |
| No guarantee that subclasses implement critical methods | Abstract methods **MUST** be implemented — compiler enforces it |
| Code duplication across similar classes | Common logic resides in concrete methods of abstract class (see [Template Method Pattern](#3-first-complete-example--payment-processing-system) example) |
| No common contract between related classes | Abstract class acts as a contract/template — all subclasses follow same interface |

**Real-world Example:** See [Section 3: Payment Processing System](#3-first-complete-example--payment-processing-system) to see how abstract classes prevent chaos when multiple developers implement different payment types.

#### **Key Rules of Abstract Classes**

> 📌 **Remember:** These rules are enforced by the SAP ABAP compiler. If you violate them, you'll get compilation errors. See [Common Faults](#5-what-happens-if-you-try-to-instantiate-an-abstract-class) to learn what happens when rules are broken.

```
╔══════════════════════════════════════════════════════════════════╗
║                   RULES OF ABSTRACT CLASSES                     ║
╠══════════════════════════════════════════════════════════════════╣
║                                                                  ║
║  RULE 1: You CANNOT create an object of an abstract class       ║
║          (See details at Section 5)                              ║
║                                                                  ║
║  RULE 2: Abstract methods have NO implementation (no body)      ║
║          (See examples at Section 3)                             ║
║                                                                  ║
║  RULE 3: If a class has even ONE abstract method, the class     ║
║          MUST be declared as ABSTRACT                            ║
║                                                                  ║
║  RULE 4: Subclasses MUST implement ALL abstract methods         ║
║          OR the subclass itself must be declared ABSTRACT        ║
║          (See practical example at Section 6)                    ║
║                                                                  ║
║  RULE 5: Abstract classes CAN have constructors                 ║
║          (See Payment Processing System at Section 3)            ║
║                                                                  ║
║  RULE 6: Abstract classes CAN have concrete (normal) methods    ║
║          (See execute_full_payment() at Section 3)               ║
║                                                                  ║
║  RULE 7: Abstract classes CAN have attributes (variables)       ║
║                                                                  ║
║  RULE 8: You CAN declare a reference variable of abstract       ║
║          class type and point it to a subclass object            ║
║          (Polymorphism - See Section 3)                          ║
║                                                                  ║
╚══════════════════════════════════════════════════════════════════╝
```

---

### **2 Syntax of Abstract Classes**

> **Quick Navigation:**  
> - See practical examples? Jump to [Payment Processing System](#3-first-complete-example--payment-processing-system)  
> - Debug abstract class errors? See [Common Faults](#5-what-happens-if-you-try-to-instantiate-an-abstract-class)

```abap
*&---------------------------------------------------------------------*
*& SYNTAX: Abstract Class Declaration
*&---------------------------------------------------------------------*

" The keyword ABSTRACT after CLASS...DEFINITION makes it abstract
CLASS lcl_abstract_class DEFINITION ABSTRACT.

  PUBLIC SECTION.

    " Abstract method - has NO body/implementation
    " Subclasses MUST implement this method
    METHODS: abstract_method ABSTRACT
      IMPORTING
        iv_input TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    " Concrete method - has FULL body/implementation
    " Subclasses inherit this method as-is (can also override)
    METHODS: concrete_method
      RETURNING
        VALUE(rv_result) TYPE string.

  PROTECTED SECTION.

    " Attributes are allowed in abstract classes
    DATA: mv_name TYPE string.

ENDCLASS.

CLASS lcl_abstract_class IMPLEMENTATION.

  " NOTE: We do NOT write implementation for abstract_method here
  " The compiler will give an error if we try to implement it

  " Only concrete methods are implemented here
  METHOD concrete_method.
    rv_result = |This is a concrete method in abstract class|.
  ENDMETHOD.

ENDCLASS.
```

---

### **3 First Complete Example — Payment Processing System**

> 🎯 **THIS IS THE MAIN EXAMPLE** - This section demonstrates everything you need to know about abstract classes in action. It covers:
> - Abstract methods with REDEFINITION in Section 3 (Credit Card Payment)  
> - Multi-level inheritance in action  
> - Polymorphism and the Template Method Pattern  
> - Real-world design patterns
>
> **Back to Basics?** See [What Is an Abstract Class?](#1-what-is-an-abstract-class)  
> **Need Syntax Details?** See [Syntax](#2-syntax-of-abstract-classes)  
> **Common Errors?** See [Common Faults](#5-what-happens-if-you-try-to-instantiate-an-abstract-class)

This is a real-world scenario. Every company processes payments, but the method varies — Bank Transfer, Credit Card, UPI, etc.

```abap
*&---------------------------------------------------------------------*
*& Report: Z_ABSTRACT_PAYMENT_DEMO
*& Purpose: Demonstrate Abstract Classes with Payment Processing
*& Author: ABAP OOP Book
*& Date: 2024
*&---------------------------------------------------------------------*
REPORT z_abstract_payment_demo.

*&---------------------------------------------------------------------*
*& ABSTRACT CLASS: LCL_PAYMENT
*& Purpose: This is the MASTER BLUEPRINT for all payment types.
*&          It defines WHAT every payment must do, but not HOW.
*&
*& WHY ABSTRACT?
*&   - Every payment MUST be validated    → abstract method
*&   - Every payment MUST be processed    → abstract method
*&   - Every payment MUST generate receipt → abstract method
*&   - But logging is SAME for all        → concrete method
*&---------------------------------------------------------------------*
CLASS lcl_payment DEFINITION ABSTRACT.

  PUBLIC SECTION.

    "---------------------------------------------------------------
    " Constructor: Even abstract classes can have constructors!
    " This constructor collects information common to ALL payments
    "---------------------------------------------------------------
    METHODS: constructor
      IMPORTING
        iv_payment_id TYPE string    " Unique payment identifier
        iv_amount     TYPE p         " Payment amount
        iv_currency   TYPE string.   " Currency code (USD, EUR, INR)

    "---------------------------------------------------------------
    " ABSTRACT METHOD: validate_payment
    " Purpose: Check if payment details are correct
    " WHY ABSTRACT: Validation rules differ for each payment type
    "   - Credit Card: Check card number, expiry, CVV
    "   - Bank Transfer: Check account number, IFSC code
    "   - UPI: Check UPI ID format
    " Each subclass MUST write its own validation logic
    "---------------------------------------------------------------
    METHODS: validate_payment ABSTRACT
      RETURNING
        VALUE(rv_is_valid) TYPE abap_bool.

    "---------------------------------------------------------------
    " ABSTRACT METHOD: process_payment
    " Purpose: Execute the actual payment
    " WHY ABSTRACT: Processing mechanism differs for each type
    "   - Credit Card: Connect to card network (Visa/Mastercard)
    "   - Bank Transfer: Connect to banking system (NEFT/SWIFT)
    "   - UPI: Connect to UPI gateway
    "---------------------------------------------------------------
    METHODS: process_payment ABSTRACT
      RETURNING
        VALUE(rv_status) TYPE string.

    "---------------------------------------------------------------
    " ABSTRACT METHOD: generate_receipt
    " Purpose: Create a payment receipt
    " WHY ABSTRACT: Receipt format/content varies by payment type
    "---------------------------------------------------------------
    METHODS: generate_receipt ABSTRACT
      RETURNING
        VALUE(rv_receipt) TYPE string.

    "---------------------------------------------------------------
    " CONCRETE METHOD: execute_full_payment
    " Purpose: Orchestrate the complete payment flow
    " WHY CONCRETE: The SEQUENCE of steps is SAME for all payments:
    "   Step 1: Log the start
    "   Step 2: Validate
    "   Step 3: Process
    "   Step 4: Generate receipt
    "   Step 5: Log the end
    " This is called the TEMPLATE METHOD PATTERN (Chapter 17)
    "---------------------------------------------------------------
    METHODS: execute_full_payment.

    "---------------------------------------------------------------
    " CONCRETE METHOD: log_payment
    " Purpose: Write payment activity to log
    " WHY CONCRETE: Logging mechanism is SAME for all payment types
    "---------------------------------------------------------------
    METHODS: log_payment
      IMPORTING
        iv_message TYPE string.

    "---------------------------------------------------------------
    " CONCRETE METHOD: get_payment_info
    " Purpose: Return formatted payment information
    "---------------------------------------------------------------
    METHODS: get_payment_info
      RETURNING
        VALUE(rv_info) TYPE string.

  PROTECTED SECTION.

    "---------------------------------------------------------------
    " Protected attributes: Accessible to this class AND subclasses
    " These store the common data that every payment needs
    "---------------------------------------------------------------
    DATA: mv_payment_id TYPE string,     " Payment identifier
          mv_amount     TYPE p LENGTH 10 DECIMALS 2, " Amount
          mv_currency   TYPE string,     " Currency
          mv_timestamp  TYPE timestamp,  " When payment was created
          mv_status     TYPE string.     " Current status

  PRIVATE SECTION.

    "---------------------------------------------------------------
    " Private attributes: Only this abstract class can access
    " Even subclasses CANNOT access these directly
    "---------------------------------------------------------------
    DATA: mv_log_count TYPE i.  " Number of log entries written

ENDCLASS.

*&---------------------------------------------------------------------*
*& IMPLEMENTATION of Abstract Class LCL_PAYMENT
*& NOTE: We implement ONLY concrete methods here.
*&       Abstract methods have NO implementation in this class.
*&---------------------------------------------------------------------*
CLASS lcl_payment IMPLEMENTATION.

  "---------------------------------------------------------------
  " Constructor Implementation
  " - Initializes common payment data
  " - Sets initial status to 'CREATED'
  " - Records the creation timestamp
  "---------------------------------------------------------------
  METHOD constructor.
    mv_payment_id = iv_payment_id.
    mv_amount     = iv_amount.
    mv_currency   = iv_currency.
    mv_status     = 'CREATED'.

    " Get current timestamp
    GET TIME STAMP FIELD mv_timestamp.

    " Log the creation
    log_payment( |Payment { mv_payment_id } created for { mv_amount } { mv_currency }| ).
  ENDMETHOD.

  "---------------------------------------------------------------
  " execute_full_payment: The Template Method
  " This method calls abstract methods — the actual implementations
  " will come from whichever subclass object is being used.
  " This is POLYMORPHISM in action! (Chapter 13)
  "---------------------------------------------------------------
  METHOD execute_full_payment.

    DATA: lv_is_valid TYPE abap_bool,
          lv_status   TYPE string,
          lv_receipt  TYPE string.

    " Step 1: Log the start of payment processing
    log_payment( |=== Starting Payment Processing ===| ).
    log_payment( get_payment_info( ) ).

    " Step 2: Validate the payment
    " This calls the ABSTRACT method — actual code is in subclass
    log_payment( |Validating payment...| ).
    lv_is_valid = validate_payment( ).

    IF lv_is_valid = abap_true.
      log_payment( |✓ Payment validation PASSED| ).

      " Step 3: Process the payment
      " This calls the ABSTRACT method — actual code is in subclass
      log_payment( |Processing payment...| ).
      lv_status = process_payment( ).
      mv_status = lv_status.
      log_payment( |Payment status: { lv_status }| ).

      " Step 4: Generate receipt
      " This calls the ABSTRACT method — actual code is in subclass
      IF lv_status = 'SUCCESS'.
        log_payment( |Generating receipt...| ).
        lv_receipt = generate_receipt( ).
        log_payment( lv_receipt ).
      ENDIF.

    ELSE.
      log_payment( |✗ Payment validation FAILED| ).
      mv_status = 'VALIDATION_FAILED'.
    ENDIF.

    " Step 5: Log the end
    log_payment( |=== Payment Processing Complete ===| ).

  ENDMETHOD.

  "---------------------------------------------------------------
  " log_payment: Write a log entry
  " Simple implementation — in production, this would write to
  " a database table, application log (SLG1), or external system
  "---------------------------------------------------------------
  METHOD log_payment.
    mv_log_count = mv_log_count + 1.
    WRITE: / |[LOG { mv_log_count }] { iv_message }|.
  ENDMETHOD.

  "---------------------------------------------------------------
  " get_payment_info: Return formatted payment information
  "---------------------------------------------------------------
  METHOD get_payment_info.
    rv_info = |Payment ID: { mv_payment_id }, | &
              |Amount: { mv_amount } { mv_currency }, | &
              |Status: { mv_status }|.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& CONCRETE SUBCLASS 1: LCL_CREDIT_CARD_PAYMENT
*& Purpose: Implements payment processing for Credit Card payments
*& This class MUST implement ALL abstract methods from LCL_PAYMENT
*&---------------------------------------------------------------------*
CLASS lcl_credit_card_payment DEFINITION
  INHERITING FROM lcl_payment.   " Inherits from abstract class

  PUBLIC SECTION.

    "---------------------------------------------------------------
    " Constructor for Credit Card Payment
    " Needs additional data: card number, expiry, CVV
    "---------------------------------------------------------------
    METHODS: constructor
      IMPORTING
        iv_payment_id  TYPE string
        iv_amount      TYPE p
        iv_currency    TYPE string
        iv_card_number TYPE string    " 16-digit card number
        iv_expiry      TYPE string    " MM/YY format
        iv_cvv         TYPE string.   " 3-digit CVV

    "---------------------------------------------------------------
    " IMPLEMENTING abstract methods (MANDATORY)
    " The keyword REDEFINITION is used to implement/override
    "---------------------------------------------------------------
    METHODS: validate_payment REDEFINITION.
    METHODS: process_payment  REDEFINITION.
    METHODS: generate_receipt  REDEFINITION.

  PRIVATE SECTION.

    " Credit card specific data
    DATA: mv_card_number TYPE string,
          mv_expiry      TYPE string,
          mv_cvv         TYPE string,
          mv_masked_card TYPE string.  " Last 4 digits only

ENDCLASS.

CLASS lcl_credit_card_payment IMPLEMENTATION.

  "---------------------------------------------------------------
  " Constructor: Initialize credit card specific data
  " SUPER->CONSTRUCTOR( ) must be called first to initialize
  " the parent abstract class
  "---------------------------------------------------------------
  METHOD constructor.

    " Call parent constructor FIRST (mandatory when parent has constructor)
    super->constructor(
      iv_payment_id = iv_payment_id
      iv_amount     = iv_amount
      iv_currency   = iv_currency
    ).

    " Store credit card specific data
    mv_card_number = iv_card_number.
    mv_expiry      = iv_expiry.
    mv_cvv         = iv_cvv.

    " Create masked card number for security (show only last 4 digits)
    " Example: 1234567890123456 → ****-****-****-3456
    DATA(lv_len) = strlen( iv_card_number ).
    IF lv_len >= 4.
      mv_masked_card = |****-****-****-{ iv_card_number+12(4) }|.
    ENDIF.

  ENDMETHOD.

  "---------------------------------------------------------------
  " validate_payment: Credit Card Specific Validation
  " Checks:
  "   1. Card number must be 16 digits
  "   2. CVV must be 3 digits
  "   3. Amount must be positive
  "   4. Expiry must not be empty
  "---------------------------------------------------------------
  METHOD validate_payment.

    rv_is_valid = abap_true.  " Assume valid, then check

    " Check 1: Card number length
    IF strlen( mv_card_number ) <> 16.
      log_payment( |ERROR: Card number must be 16 digits| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    " Check 2: CVV length
    IF strlen( mv_cvv ) <> 3.
      log_payment( |ERROR: CVV must be 3 digits| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    " Check 3: Amount must be positive
    IF mv_amount <= 0.
      log_payment( |ERROR: Amount must be greater than zero| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    " Check 4: Expiry must not be empty
    IF mv_expiry IS INITIAL.
      log_payment( |ERROR: Expiry date is required| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    log_payment( |Card { mv_masked_card } validated successfully| ).

  ENDMETHOD.

  "---------------------------------------------------------------
  " process_payment: Credit Card Specific Processing
  " In real life, this would:
  "   1. Connect to payment gateway (Visa/Mastercard network)
  "   2. Send encrypted card data
  "   3. Wait for authorization response
  "   4. Return success/failure
  " Here we simulate the process
  "---------------------------------------------------------------
  METHOD process_payment.

    log_payment( |Connecting to Credit Card payment gateway...| ).
    log_payment( |Sending encrypted data for card { mv_masked_card }...| ).
    log_payment( |Authorization received from card network| ).
    log_payment( |Charging { mv_amount } { mv_currency } to card { mv_masked_card }| ).

    " Simulate success
    rv_status = 'SUCCESS'.

  ENDMETHOD.

  "---------------------------------------------------------------
  " generate_receipt: Credit Card Specific Receipt
  "---------------------------------------------------------------
  METHOD generate_receipt.

    rv_receipt = |╔══════════════════════════════════════╗| && cl_abap_char_utilities=>newline &&
                 |║     CREDIT CARD PAYMENT RECEIPT      ║| && cl_abap_char_utilities=>newline &&
                 |╠══════════════════════════════════════╣| && cl_abap_char_utilities=>newline &&
                 |║ Payment ID: { mv_payment_id }| && cl_abap_char_utilities=>newline &&
                 |║ Card: { mv_masked_card }| && cl_abap_char_utilities=>newline &&
                 |║ Amount: { mv_amount } { mv_currency }| && cl_abap_char_utilities=>newline &&
                 |║ Status: APPROVED| && cl_abap_char_utilities=>newline &&
                 |╚══════════════════════════════════════╝|.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& CONCRETE SUBCLASS 2: LCL_BANK_TRANSFER_PAYMENT
*& Purpose: Implements payment processing for Bank Transfer payments
*&---------------------------------------------------------------------*
CLASS lcl_bank_transfer_payment DEFINITION
  INHERITING FROM lcl_payment.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        iv_payment_id    TYPE string
        iv_amount        TYPE p
        iv_currency      TYPE string
        iv_account_no    TYPE string    " Bank account number
        iv_ifsc_code     TYPE string    " Bank IFSC/SWIFT code
        iv_account_name  TYPE string.   " Account holder name

    " MUST implement all abstract methods
    METHODS: validate_payment REDEFINITION.
    METHODS: process_payment  REDEFINITION.
    METHODS: generate_receipt  REDEFINITION.

  PRIVATE SECTION.

    DATA: mv_account_no   TYPE string,
          mv_ifsc_code    TYPE string,
          mv_account_name TYPE string.

ENDCLASS.

CLASS lcl_bank_transfer_payment IMPLEMENTATION.

  METHOD constructor.

    " Call parent constructor first
    super->constructor(
      iv_payment_id = iv_payment_id
      iv_amount     = iv_amount
      iv_currency   = iv_currency
    ).

    mv_account_no   = iv_account_no.
    mv_ifsc_code    = iv_ifsc_code.
    mv_account_name = iv_account_name.

  ENDMETHOD.

  "---------------------------------------------------------------
  " validate_payment: Bank Transfer Specific Validation
  " Different rules than Credit Card!
  "   1. Account number must not be empty
  "   2. IFSC code must be 11 characters (Indian banking standard)
  "   3. Account holder name must not be empty
  "---------------------------------------------------------------
  METHOD validate_payment.

    rv_is_valid = abap_true.

    IF mv_account_no IS INITIAL.
      log_payment( |ERROR: Account number is required| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    IF strlen( mv_ifsc_code ) <> 11.
      log_payment( |ERROR: IFSC code must be 11 characters| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    IF mv_account_name IS INITIAL.
      log_payment( |ERROR: Account holder name is required| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    IF mv_amount <= 0.
      log_payment( |ERROR: Amount must be greater than zero| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    log_payment( |Bank account { mv_account_no } validated successfully| ).

  ENDMETHOD.

  "---------------------------------------------------------------
  " process_payment: Bank Transfer Specific Processing
  " Completely different from Credit Card processing!
  "---------------------------------------------------------------
  METHOD process_payment.

    log_payment( |Connecting to banking network (NEFT/RTGS)...| ).
    log_payment( |Verifying account { mv_account_no } at { mv_ifsc_code }...| ).
    log_payment( |Initiating transfer of { mv_amount } { mv_currency }...| ).
    log_payment( |Transfer to { mv_account_name } initiated| ).

    rv_status = 'SUCCESS'.

  ENDMETHOD.

  "---------------------------------------------------------------
  " generate_receipt: Bank Transfer Specific Receipt
  "---------------------------------------------------------------
  METHOD generate_receipt.

    rv_receipt = |╔══════════════════════════════════════╗| && cl_abap_char_utilities=>newline &&
                 |║     BANK TRANSFER RECEIPT            ║| && cl_abap_char_utilities=>newline &&
                 |╠══════════════════════════════════════╣| && cl_abap_char_utilities=>newline &&
                 |║ Payment ID: { mv_payment_id }| && cl_abap_char_utilities=>newline &&
                 |║ Account: { mv_account_no }| && cl_abap_char_utilities=>newline &&
                 |║ IFSC: { mv_ifsc_code }| && cl_abap_char_utilities=>newline &&
                 |║ Name: { mv_account_name }| && cl_abap_char_utilities=>newline &&
                 |║ Amount: { mv_amount } { mv_currency }| && cl_abap_char_utilities=>newline &&
                 |║ Status: TRANSFERRED| && cl_abap_char_utilities=>newline &&
                 |╚══════════════════════════════════════╝|.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& CONCRETE SUBCLASS 3: LCL_UPI_PAYMENT
*& Purpose: Implements payment processing for UPI payments
*&---------------------------------------------------------------------*
CLASS lcl_upi_payment DEFINITION
  INHERITING FROM lcl_payment.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        iv_payment_id TYPE string
        iv_amount     TYPE p
        iv_currency   TYPE string
        iv_upi_id     TYPE string.    " UPI ID like user@bank

    METHODS: validate_payment REDEFINITION.
    METHODS: process_payment  REDEFINITION.
    METHODS: generate_receipt  REDEFINITION.

  PRIVATE SECTION.

    DATA: mv_upi_id TYPE string.

ENDCLASS.

CLASS lcl_upi_payment IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      iv_payment_id = iv_payment_id
      iv_amount     = iv_amount
      iv_currency   = iv_currency
    ).
    mv_upi_id = iv_upi_id.
  ENDMETHOD.

  "---------------------------------------------------------------
  " validate_payment: UPI Specific Validation
  " UPI ID must contain '@' symbol (e.g., user@okbank)
  "---------------------------------------------------------------
  METHOD validate_payment.

    rv_is_valid = abap_true.

    " UPI ID must contain '@'
    IF mv_upi_id NS '@'.      " NS = Not contains String
      log_payment( |ERROR: Invalid UPI ID format. Must contain @| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    " UPI has a transaction limit (simulated: 100000)
    IF mv_amount > 100000.
      log_payment( |ERROR: UPI limit exceeded. Max 100,000| ).
      rv_is_valid = abap_false.
      RETURN.
    ENDIF.

    log_payment( |UPI ID { mv_upi_id } validated successfully| ).

  ENDMETHOD.

  METHOD process_payment.

    log_payment( |Connecting to UPI payment gateway...| ).
    log_payment( |Sending payment request to { mv_upi_id }...| ).
    log_payment( |UPI payment of { mv_amount } { mv_currency } completed| ).

    rv_status = 'SUCCESS'.

  ENDMETHOD.

  METHOD generate_receipt.

    rv_receipt = |╔══════════════════════════════════════╗| && cl_abap_char_utilities=>newline &&
                 |║     UPI PAYMENT RECEIPT              ║| && cl_abap_char_utilities=>newline &&
                 |╠══════════════════════════════════════╣| && cl_abap_char_utilities=>newline &&
                 |║ Payment ID: { mv_payment_id }| && cl_abap_char_utilities=>newline &&
                 |║ UPI ID: { mv_upi_id }| && cl_abap_char_utilities=>newline &&
                 |║ Amount: { mv_amount } { mv_currency }| && cl_abap_char_utilities=>newline &&
                 |║ Status: COMPLETED| && cl_abap_char_utilities=>newline &&
                 |╚══════════════════════════════════════╝|.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& MAIN PROGRAM: Demonstrate all three payment types
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  " Declare a reference variable of the ABSTRACT class type
  " This is possible! We just cannot CREATE OBJECT of abstract class.
  DATA: lo_payment TYPE REF TO lcl_payment.

  WRITE: / |{ '=' WIDTH = 60 PAD = '=' }|.
  WRITE: / |       PAYMENT PROCESSING SYSTEM DEMO|.
  WRITE: / |{ '=' WIDTH = 60 PAD = '=' }|.
  SKIP 1.

  "---------------------------------------------------------------
  " PAYMENT 1: Credit Card Payment
  " Notice: We CREATE OBJECT of the CONCRETE subclass
  "         but store it in the ABSTRACT class reference variable
  "---------------------------------------------------------------
  WRITE: / |--- CREDIT CARD PAYMENT ---| COLOR COL_HEADING.

  CREATE OBJECT lo_payment
    TYPE lcl_credit_card_payment
    EXPORTING
      iv_payment_id  = 'PAY-CC-001'
      iv_amount      = '1500.00'
      iv_currency    = 'USD'
      iv_card_number = '4111111111111234'
      iv_expiry      = '12/25'
      iv_cvv         = '456'.

  " Call execute_full_payment — it will use Credit Card implementations
  lo_payment->execute_full_payment( ).

  SKIP 2.

  "---------------------------------------------------------------
  " PAYMENT 2: Bank Transfer Payment
  " Same reference variable, different object type!
  "---------------------------------------------------------------
  WRITE: / |--- BANK TRANSFER PAYMENT ---| COLOR COL_POSITIVE.

  CREATE OBJECT lo_payment
    TYPE lcl_bank_transfer_payment
    EXPORTING
      iv_payment_id   = 'PAY-BT-002'
      iv_amount       = '50000.00'
      iv_currency     = 'INR'
      iv_account_no   = '1234567890'
      iv_ifsc_code    = 'SBIN0001234'
      iv_account_name = 'Rajesh Kumar'.

  lo_payment->execute_full_payment( ).

  SKIP 2.

  "---------------------------------------------------------------
  " PAYMENT 3: UPI Payment
  "---------------------------------------------------------------
  WRITE: / |--- UPI PAYMENT ---| COLOR COL_GROUP.

  CREATE OBJECT lo_payment
    TYPE lcl_upi_payment
    EXPORTING
      iv_payment_id = 'PAY-UPI-003'
      iv_amount     = '2500.00'
      iv_currency   = 'INR'
      iv_upi_id     = 'rajesh@okicici'.

  lo_payment->execute_full_payment( ).
```

#### **Output Explanation**
When you run this program, you will see three complete payment processing flows. Each payment type goes through the same sequence (validate → process → receipt) but uses its own specific logic for each step.

---

### **4 Abstract Class vs Regular Class — Detailed Comparison**

> **Context:** Confused about which type to use? This section helps you decide between **Abstract Classes**, **Regular Classes**, and **Final Classes**.  
> Full decision guide available at [Section 11](#11-when-to-use-abstract-class-vs-regular-class-vs-final-class)

```
╔═══════════════════════════╦═══════════════════════════════════════╗
║      FEATURE              ║   ABSTRACT CLASS    vs  REGULAR CLASS ║
╠═══════════════════════════╬═══════════════════════════════════════╣
║ Can create objects?       ║   ❌ NO              ✅ YES           ║
║ Can have abstract methods?║   ✅ YES             ❌ NO            ║
║ Can have concrete methods?║   ✅ YES             ✅ YES           ║
║ Can have attributes?      ║   ✅ YES             ✅ YES           ║
║ Can have constructor?     ║   ✅ YES             ✅ YES           ║
║ Can be inherited?         ║   ✅ YES (purpose!)  ✅ YES           ║
║ Forces implementation?    ║   ✅ YES (abstract   ❌ NO            ║
║                           ║   methods)                            ║
║ Can have static methods?  ║   ✅ YES             ✅ YES           ║
║ Reference variable?       ║   ✅ YES             ✅ YES           ║
╚═══════════════════════════╩═══════════════════════════════════════╝
```

**Key Difference Summary:**
- **Use ABSTRACT CLASS when:** You want to define a template that FORCES subclasses to implement certain methods
- **Use REGULAR CLASS when:** You want normal inheritance without enforcement
- **Use FINAL CLASS when:** You want to prevent inheritance entirely

**See Also:**
- [Payment Processing System Example](#3-first-complete-example--payment-processing-system) - Shows real use of abstract class vs regular class behavior
- [Decision Guide](#11-when-to-use-abstract-class-vs-regular-class-vs-final-class) - Detailed flowchart for choosing the right class type

---

### **5 What Happens If You Try to Instantiate an Abstract Class?**

> ⚠️ **COMPILATION ERROR SECTION** - Learn what errors you'll get and how to fix them  
> **Common Faults:** This section shows the **WRONG way** and the **CORRECT way** to work with abstract classes

*Related Sections:*  
- [Rules of Abstract Classes](#key-rules-of-abstract-classes) - The rules you're breaking  
- [Multi-Level Inheritance](#7-multi-level-abstract-inheritance) - Advanced inheritance patterns  
- [Best Practices](#10-industry-best-practices-for-abstract-classes) - How to avoid these errors

```abap
*&---------------------------------------------------------------------*
*& COMMON FAULT #1: Trying to create object of abstract class
*& This will give a COMPILATION ERROR
*&---------------------------------------------------------------------*

CLASS lcl_animal DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: make_sound ABSTRACT
      RETURNING VALUE(rv_sound) TYPE string.
ENDCLASS.

" There is NO implementation section needed if ALL methods are abstract

START-OF-SELECTION.

  DATA: lo_animal TYPE REF TO lcl_animal.

  " ❌ THIS WILL GIVE ERROR:
  " "Abstract class 'LCL_ANIMAL' cannot be instantiated"
  CREATE OBJECT lo_animal.    " <-- COMPILATION ERROR!

  " ✅ CORRECT WAY: Create object of a concrete subclass
  " CREATE OBJECT lo_animal TYPE lcl_dog.
```

**Error Message:**
```
❌ "You cannot create an instance of the abstract class LCL_ANIMAL"
```

---

### **6 What Happens If Subclass Doesn't Implement All Abstract Methods?**

> 🐛 **DEBUGGING GUIDE** - This is a very common error when working with abstract classes  
> **2 Solutions Provided:**
> 1. Implement the missing method (Fix the subclass)
> 2. Make the subclass abstract too (Defer implementation to next level)
>
> **When to use each solution?** See [Section 7: Multi-Level Inheritance](#7-multi-level-abstract-inheritance)

**Also See:**
- [Exercise: Employee Salary System](#9-exercise-1-employee-salary-system) - Practical exercise where all methods must be implemented  
- [Multi-Level Inheritance](#7-multi-level-abstract-inheritance) - When to use Solution 2 (deferred implementation)

```abap
*&---------------------------------------------------------------------*
*& COMMON FAULT #2: Not implementing all abstract methods
*& This will give a COMPILATION ERROR
*&---------------------------------------------------------------------*

CLASS lcl_shape DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: calculate_area ABSTRACT
      RETURNING VALUE(rv_area) TYPE f.
    METHODS: calculate_perimeter ABSTRACT
      RETURNING VALUE(rv_perimeter) TYPE f.
    METHODS: get_shape_name ABSTRACT
      RETURNING VALUE(rv_name) TYPE string.
ENDCLASS.

" This subclass only implements 2 out of 3 abstract methods
CLASS lcl_circle DEFINITION INHERITING FROM lcl_shape.
  PUBLIC SECTION.
    METHODS: calculate_area REDEFINITION.
    METHODS: calculate_perimeter REDEFINITION.
    " ❌ MISSING: get_shape_name is NOT redefined!
    " This will cause a COMPILATION ERROR
ENDCLASS.

CLASS lcl_circle IMPLEMENTATION.
  METHOD calculate_area.
    " implementation
  ENDMETHOD.
  METHOD calculate_perimeter.
    " implementation
  ENDMETHOD.
  " ❌ get_shape_name is missing!
ENDCLASS.
```

**Error Message:**
```
❌ "Class LCL_CIRCLE must be ABSTRACT since it does not implement 
    method GET_SHAPE_NAME from class LCL_SHAPE"
```

#### **Solution 1: Implement the missing method**
```abap
" Add this to LCL_CIRCLE DEFINITION:
METHODS: get_shape_name REDEFINITION.

" Add this to LCL_CIRCLE IMPLEMENTATION:
METHOD get_shape_name.
  rv_name = 'Circle'.
ENDMETHOD.
```

#### **Solution 2: Make the subclass ALSO abstract (if appropriate)**
```abap
" If you intentionally don't want to implement all methods yet:
CLASS lcl_circle DEFINITION ABSTRACT INHERITING FROM lcl_shape.
  " Now lcl_circle is also abstract
  " Another class must inherit from lcl_circle and implement remaining methods
ENDCLASS.
```

---

### **7 Multi-Level Abstract Inheritance**
Sometimes you have a hierarchy where abstract classes inherit from other abstract classes.

> **Advanced Topic:** This section covers:
> - **LEVEL 1:** Most general abstract class (LCL_VEHICLE)
> - **LEVEL 2:** Still abstract, but implements some methods (LCL_MOTOR_VEHICLE - see [Solution 2 from Section 6](#what-happens-if-subclass-doesnt-implement-all-abstract-methods))
> - **LEVEL 3:** Fully concrete implementation (LCL_CAR)
>
> **When You Need This:** When you have related concepts at different abstraction levels  
> **Real Example:** The [Payment Processing System](#3-first-complete-example--payment-processing-system) shows 1 abstract + 3 concrete (no intermediate abstract)

```abap
*&---------------------------------------------------------------------*
*& EXAMPLE: Multi-Level Abstract Hierarchy
*& Level 1: lcl_vehicle (abstract) — most general
*& Level 2: lcl_motor_vehicle (abstract) — adds engine details
*& Level 3: lcl_car (concrete) — specific implementation
*&---------------------------------------------------------------------*
REPORT z_multi_level_abstract.

*----------------------------------------------------------------------*
* LEVEL 1: The most general abstract class
*----------------------------------------------------------------------*
CLASS lcl_vehicle DEFINITION ABSTRACT.
  PUBLIC SECTION.
    " Every vehicle must be able to move — but HOW varies
    METHODS: move ABSTRACT
      RETURNING VALUE(rv_action) TYPE string.

    " Every vehicle has a type
    METHODS: get_vehicle_type ABSTRACT
      RETURNING VALUE(rv_type) TYPE string.

    " Common to all vehicles
    METHODS: display_info.

  PROTECTED SECTION.
    DATA: mv_color TYPE string.
ENDCLASS.

CLASS lcl_vehicle IMPLEMENTATION.
  METHOD display_info.
    WRITE: / |Vehicle Color: { mv_color }|.
    WRITE: / |Type: { get_vehicle_type( ) }|.
    WRITE: / |Movement: { move( ) }|.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* LEVEL 2: Still abstract — adds engine concept
* Implements get_vehicle_type but leaves move() abstract
* Also adds a NEW abstract method: start_engine
*----------------------------------------------------------------------*
CLASS lcl_motor_vehicle DEFINITION ABSTRACT
  INHERITING FROM lcl_vehicle.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        iv_color       TYPE string
        iv_engine_type TYPE string.

    " Implements ONE of the parent's abstract methods
    METHODS: get_vehicle_type REDEFINITION.

    " NOTE: move() is STILL abstract — NOT implemented here
    " So lcl_motor_vehicle must remain ABSTRACT

    " NEW abstract method specific to motor vehicles
    METHODS: start_engine ABSTRACT
      RETURNING VALUE(rv_message) TYPE string.

  PROTECTED SECTION.
    DATA: mv_engine_type TYPE string.

ENDCLASS.

CLASS lcl_motor_vehicle IMPLEMENTATION.

  METHOD constructor.
    " Cannot call super->constructor here because lcl_vehicle
    " does not have an explicit constructor
    mv_color       = iv_color.
    mv_engine_type = iv_engine_type.
  ENDMETHOD.

  METHOD get_vehicle_type.
    rv_type = |Motor Vehicle (Engine: { mv_engine_type })|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LEVEL 3: Concrete class — implements ALL remaining abstract methods
* Must implement: move() from lcl_vehicle
*                 start_engine() from lcl_motor_vehicle
*----------------------------------------------------------------------*
CLASS lcl_car DEFINITION
  INHERITING FROM lcl_motor_vehicle.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        iv_color       TYPE string
        iv_engine_type TYPE string
        iv_brand       TYPE string.

    " Implement ALL remaining abstract methods
    METHODS: move REDEFINITION.
    METHODS: start_engine REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_brand TYPE string.

ENDCLASS.

CLASS lcl_car IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      iv_color       = iv_color
      iv_engine_type = iv_engine_type
    ).
    mv_brand = iv_brand.
  ENDMETHOD.

  METHOD move.
    rv_action = |{ mv_brand } car drives on roads at high speed|.
  ENDMETHOD.

  METHOD start_engine.
    rv_message = |{ mv_brand } { mv_engine_type } engine started: VROOM!|.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Can use any level's reference type for polymorphism
  DATA: lo_vehicle TYPE REF TO lcl_vehicle.

  CREATE OBJECT lo_vehicle
    TYPE lcl_car
    EXPORTING
      iv_color       = 'Red'
      iv_engine_type = 'V8 Turbo'
      iv_brand       = 'BMW'.

  lo_vehicle->display_info( ).

  SKIP 1.

  " Can also use the middle level's reference type
  DATA: lo_motor TYPE REF TO lcl_motor_vehicle.

  lo_motor ?= lo_vehicle.  " Narrowing cast (safe because it IS a motor vehicle)
  WRITE: / lo_motor->start_engine( ).
```

---

### **8 Abstract Classes with Static Methods**

> **📌 IMPORTANT:** Static methods in abstract classes behave **differently** than instance methods  
> 
> **Key Difference:**
> - **Instance Methods AND Abstract Methods:** Require object creation → Must be implemented in concrete subclass
> - **Static Methods:** Can be called WITHOUT creating an object → Can be called directly on abstract class itself!
>
> **Practical Use:** Utility methods common to all subclasses (e.g., format_currency() that all payment types use)

CLASS lcl_utility DEFINITION ABSTRACT.
  PUBLIC SECTION.

    " Static method — can be called without object
    CLASS-METHODS: format_currency
      IMPORTING
        iv_amount   TYPE p
        iv_currency TYPE string
      RETURNING
        VALUE(rv_formatted) TYPE string.

    " Abstract instance method — must be implemented by subclass
    METHODS: do_something ABSTRACT.

ENDCLASS.

CLASS lcl_utility IMPLEMENTATION.
  METHOD format_currency.
    rv_formatted = |{ iv_amount } { iv_currency }|.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  " ✅ You CAN call static methods of abstract class directly!
  DATA(lv_result) = lcl_utility=>format_currency(
    iv_amount   = '1500.50'
    iv_currency = 'EUR'
  ).

  WRITE: / lv_result.   " Output: 1,500.50 EUR
```

---

### **9 Exercise 1: Employee Salary System**

> 📝 **HANDS-ON PRACTICE** - Comprehensive exercise that covers:
> - Abstract class with attributes, constructor, and mixed methods
> - 3 concrete subclasses with different implementations  
> - Using abstract class reference for polymorphism
> - Table of objects with polymorphic calls
>
> **Video Walkthrough Concepts:**
> - Similar to [Payment Processing System](#3-first-complete-example--payment-processing-system) but focuses on salary calculations
> - Uses the same design patterns as the Payment example
> - Tests your understanding of [All Rules](#key-rules-of-abstract-classes)
>
> **Difficulty:** ⭐⭐⭐ (Intermediate - All concepts combined)  
> **Time:** 30-45 minutes to code and test

#### **Problem Statement**
Create an abstract class `LCL_EMPLOYEE` with:
- Attributes: employee ID, name, base salary
- Abstract method: `calculate_salary` (returns total salary)
- Abstract method: `get_designation` (returns job title)
- Concrete method: `display_payslip` (prints formatted payslip)

Create three concrete subclasses:
- `LCL_MANAGER` — salary = base + 30% bonus + 15000 allowance
- `LCL_DEVELOPER` — salary = base + 20% bonus + 10000 allowance
- `LCL_INTERN` — salary = base (no bonus, no allowance)

#### **Solution**
```abap
*&---------------------------------------------------------------------*
*& Exercise Solution: Employee Salary System
*&---------------------------------------------------------------------*
REPORT z_exercise_abstract_employee.

*----------------------------------------------------------------------*
* ABSTRACT CLASS: LCL_EMPLOYEE
*----------------------------------------------------------------------*
CLASS lcl_employee DEFINITION ABSTRACT.

  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING
        iv_emp_id      TYPE string
        iv_name        TYPE string
        iv_base_salary TYPE p.

    " Abstract: Each employee type calculates salary differently
    METHODS: calculate_salary ABSTRACT
      RETURNING VALUE(rv_total_salary) TYPE p.

    " Abstract: Each employee type has a different designation
    METHODS: get_designation ABSTRACT
      RETURNING VALUE(rv_designation) TYPE string.

    " Concrete: Payslip format is same for all
    METHODS: display_payslip.

  PROTECTED SECTION.
    DATA: mv_emp_id      TYPE string,
          mv_name        TYPE string,
          mv_base_salary TYPE p LENGTH 10 DECIMALS 2.

ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.

  METHOD constructor.
    mv_emp_id      = iv_emp_id.
    mv_name        = iv_name.
    mv_base_salary = iv_base_salary.
  ENDMETHOD.

  METHOD display_payslip.
    DATA(lv_total) = calculate_salary( ).    " Calls subclass implementation!
    DATA(lv_desig) = get_designation( ).     " Calls subclass implementation!

    WRITE: / |╔══════════════════════════════════════╗|.
    WRITE: / |║         EMPLOYEE PAYSLIP             ║|.
    WRITE: / |╠══════════════════════════════════════╣|.
    WRITE: / |║ ID:          { mv_emp_id }|.
    WRITE: / |║ Name:        { mv_name }|.
    WRITE: / |║ Designation: { lv_desig }|.
    WRITE: / |║ Base Salary: { mv_base_salary }|.
    WRITE: / |║ Total Salary:{ lv_total }|.
    WRITE: / |╚══════════════════════════════════════╝|.
    SKIP 1.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CONCRETE: LCL_MANAGER
*----------------------------------------------------------------------*
CLASS lcl_manager DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    METHODS: calculate_salary REDEFINITION.
    METHODS: get_designation  REDEFINITION.
ENDCLASS.

CLASS lcl_manager IMPLEMENTATION.
  METHOD calculate_salary.
    " Manager: Base + 30% bonus + 15000 allowance
    DATA(lv_bonus)     = mv_base_salary * '0.30'.
    DATA(lv_allowance) = 15000.
    rv_total_salary    = mv_base_salary + lv_bonus + lv_allowance.
  ENDMETHOD.

  METHOD get_designation.
    rv_designation = 'Senior Manager'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CONCRETE: LCL_DEVELOPER
*----------------------------------------------------------------------*
CLASS lcl_developer DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    METHODS: calculate_salary REDEFINITION.
    METHODS: get_designation  REDEFINITION.
ENDCLASS.

CLASS lcl_developer IMPLEMENTATION.
  METHOD calculate_salary.
    " Developer: Base + 20% bonus + 10000 allowance
    DATA(lv_bonus)     = mv_base_salary * '0.20'.
    DATA(lv_allowance) = 10000.
    rv_total_salary    = mv_base_salary + lv_bonus + lv_allowance.
  ENDMETHOD.

  METHOD get_designation.
    rv_designation = 'Software Developer'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* CONCRETE: LCL_INTERN
*----------------------------------------------------------------------*
CLASS lcl_intern DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    METHODS: calculate_salary REDEFINITION.
    METHODS: get_designation  REDEFINITION.
ENDCLASS.

CLASS lcl_intern IMPLEMENTATION.
  METHOD calculate_salary.
    " Intern: Only base salary, no bonus, no allowance
    rv_total_salary = mv_base_salary.
  ENDMETHOD.

  METHOD get_designation.
    rv_designation = 'Intern'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Using abstract class reference for all employees (polymorphism)
  DATA: lt_employees TYPE TABLE OF REF TO lcl_employee.
  DATA: lo_emp TYPE REF TO lcl_employee.

  " Create Manager
  CREATE OBJECT lo_emp TYPE lcl_manager
    EXPORTING iv_emp_id = 'EMP001' iv_name = 'Priya Sharma' iv_base_salary = '80000'.
  APPEND lo_emp TO lt_employees.

  " Create Developer
  CREATE OBJECT lo_emp TYPE lcl_developer
    EXPORTING iv_emp_id = 'EMP002' iv_name = 'Amit Patel' iv_base_salary = '60000'.
  APPEND lo_emp TO lt_employees.

  " Create Intern
  CREATE OBJECT lo_emp TYPE lcl_intern
    EXPORTING iv_emp_id = 'INT001' iv_name = 'Ravi Singh' iv_base_salary = '15000'.
  APPEND lo_emp TO lt_employees.

  " Display payslips for ALL employees using loop
  " Each employee's payslip uses ITS OWN salary calculation
  LOOP AT lt_employees INTO lo_emp.
    lo_emp->display_payslip( ).
  ENDLOOP.
```

---

### **10 Industry Best Practices for Abstract Classes**

> ✅ **PROFESSIONAL GUIDELINES** - Learn how experienced developers use abstract classes in enterprise SAP systems
>
> **Key Areas Covered:**
> - Naming conventions (❌ WRONG vs ✅ RIGHT)
> - Code duplication prevention  
> - Template Method Pattern  
> - When to use INTERFACE instead (Chapter 12)  
> - Single Responsibility Principle
>
> **Cross-Reference:**  
> - [Section 11: Decision Guide](#11-when-to-use-abstract-class-vs-regular-class-vs-final-class) - How to choose the right approach  
> - [Real Example: Payment System](#3-first-complete-example--payment-processing-system) - Demonstrates many of these practices

```
╔══════════════════════════════════════════════════════════════════════╗
║              INDUSTRY BEST PRACTICES                                 ║
╠══════════════════════════════════════════════════════════════════════╣
║                                                                      ║
║ 1. USE abstract classes when you have a "IS-A" relationship         ║
║    Example: A Dog IS-A Animal, A Car IS-A Vehicle                   ║
║                                                                      ║
║ 2. PUT common logic in concrete methods of the abstract class       ║
║    to avoid code duplication across subclasses                       ║
║                                                                      ║
║ 3. USE the Template Method Pattern: Define the algorithm             ║
║    structure in a concrete method, but delegate specific steps       ║
║    to abstract methods                                               ║
║                                                                      ║
║ 4. NAME abstract classes clearly:                                    ║
║    ✅ LCL_ABSTRACT_PAYMENT or LCL_PAYMENT_BASE                       ║
║    ❌ LCL_PAYMENT (ambiguous — is it abstract or concrete?)          ║
║                                                                      ║
║ 5. DOCUMENT why a class is abstract — what design decision          ║
║    led to making it abstract                                         ║
║                                                                      ║
║ 6. AVOID making a class abstract if it has no abstract methods      ║
║    Use FINAL instead if you want to prevent inheritance             ║
║                                                                      ║
║ 7. PREFER interfaces over abstract classes when you only need       ║
║    to define a contract without shared implementation               ║
║    (More on this in Chapter 12)                                      ║
║                                                                      ║
║ 8. KEEP abstract classes focused — don't put too many               ║
║    responsibilities in one abstract class (Single Responsibility)   ║
║                                                                      ║
╚══════════════════════════════════════════════════════════════════════╝
```

---

### **11 When to Use Abstract Class vs Regular Class vs Final Class**

```
Decision Guide:
═══════════════

Question 1: Should this class be used as a template for other classes?
  → YES → Consider ABSTRACT CLASS
  → NO  → Go to Question 2

Question 2: Should other classes be able to inherit from this class?
  → YES → Use REGULAR CLASS
  → NO  → Use FINAL CLASS (CLASS lcl_x DEFINITION FINAL)

Question 3: Does the template have common code to share with subclasses?
  → YES → Use ABSTRACT CLASS (can have concrete methods)
  → NO  → Consider INTERFACE instead (Chapter 12)

Question 4: Do you need to enforce that subclasses implement certain methods?
  → YES → Use ABSTRACT METHODS in the abstract class
  → NO  → Use regular methods that can be optionally overridden
```

---

### **12 Chapter Summary**

```
╔══════════════════════════════════════════════════════════════════╗
║                   CHAPTER 11 SUMMARY                             ║
╠══════════════════════════════════════════════════════════════════╣
║                                                                  ║
║ • Abstract classes CANNOT be instantiated (no CREATE OBJECT)    ║
║ • Abstract methods have NO implementation body                  ║
║ • Subclasses MUST implement all abstract methods                ║
║ • Abstract classes CAN have concrete methods with full code     ║
║ • Abstract classes CAN have attributes and constructors         ║
║ • Use abstract classes for IS-A relationships with shared code  ║
║ • Template Method Pattern is a powerful use of abstract classes  ║
║ • Static methods in abstract classes CAN be called directly     ║
║ • Multi-level abstract hierarchies are possible                 ║
║                                                                  ║
╚══════════════════════════════════════════════════════════════════╝
```

---

## **Key Takeaways:**

1. **Abstract classes provide structure and enforce consistency** across related classes.
2. **They combine mandatory contracts** (abstract methods) with **reusable implementations** (concrete methods).
3. **The Template Method Pattern** is a common and powerful application of abstract classes.
4. **Proper use of abstract classes** leads to cleaner, more maintainable, and more scalable code.
5. **Always document** why you're making a class abstract — it's an important design decision that affects all subclasses.
