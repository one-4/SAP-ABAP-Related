# SAP ABAP Object Oriented Programming
## A Complete Practical Guide


---

# TABLE OF CONTENTS

## [CHAPTER 1: INTRODUCTION TO OBJECT ORIENTED PROGRAMMING](#chapter-1-introduction-to-object-oriented-programming)
  - [1.1 What is Object Oriented Programming?](#11-what-is-object-oriented-programming)
    - Real-World Analogy
  - [1.2 Why OOP in ABAP?](#12-why-oop-in-abap)
    - Traditional Procedural ABAP Problems
    - OOP Solution
    - Benefits of OOP
  - [1.3 Four Pillars of OOP](#13-four-pillars-of-oop)
  - [1.4 OOP Terminology in ABAP](#14-oop-terminology-in-abap)
  - [1.5 First OOP Program in ABAP](#15-first-oop-program-in-abap)
    - Code Explanation

## [CHAPTER 2: CLASSES - THE BLUEPRINT OF OBJECTS](#chapter-2-classes--the-blueprint-of-objects)
  - [2.1 Understanding Classes](#21-understanding-classes)
  - [2.2 Types of Classes in ABAP](#22-types-of-classes-in-abap)
    - 2.2.1 Local Classes
    - 2.2.2 Global Classes
  - [2.3 Naming Conventions (Industry Standards)](#23-naming-conventions-industry-standards)
  - [2.4 Class Attributes](#24-class-attributes)
    - Types of Attributes
  - [2.5 Complete Example: Bank Account Class](#25-complete-example-bank-account-class)
  - [2.6 Exercise 1: Create a Student Class](#26-exercise-1-create-a-student-class)
    - Problem Statement
    - Solution

## [CHAPTER 3: OBJECTS - BRINGING CLASSES TO LIFE](#chapter-3-objects--bringing-classes-to-life)
  - [3.1 Understanding Objects](#31-understanding-objects)
  - [3.2 Creating Objects in ABAP](#32-creating-objects-in-abap)
    - Method 1: Using CREATE OBJECT (Classic)
    - Method 2: Using NEW Operator (ABAP 7.40+)
  - [3.3 Reference Variables](#33-reference-variables)

## [CHAPTER 4: ATTRIBUTES - DATA OF A CLASS](#chapter-4-attributes--data-of-a-class)
  - [4.1 Types of Attributes](#41-types-of-attributes)
  - [4.2 Instance Attributes](#42-instance-attributes)
  - [4.3 Access Modifiers](#43-access-modifiers)
  - [4.4 Working with Attributes - Practical Examples](#44-working-with-attributes--practical-examples)

## [CHAPTER 5: METHODS - BEHAVIOR OF A CLASS](#chapter-5-methods--behavior-of-a-class)
  - [5.1 Understanding Methods](#51-understanding-methods)
  - [5.2 Method Declaration](#52-method-declaration)
  - [5.3 Method Parameters](#53-method-parameters)
  - [5.4 Return Values](#54-return-values)
  - [5.5 Practical Method Examples](#55-practical-method-examples)
  - [5.6 Exercise 2](#56-exercise-2)

## [CHAPTER 6: CONSTRUCTORS - OBJECT INITIALIZATION](#chapter-6-constructors--object-initialization)
  - [6.1 What is a Constructor?](#61-what-is-a-constructor)
  - [6.2 Constructor Features](#62-constructor-features)
  - [6.3 Default Constructor](#63-default-constructor)
  - [6.4 Parameterized Constructor](#64-parameterized-constructor)
  - [6.5 Constructor Best Practices](#65-constructor-best-practices)
  - [6.6 Complete Example with Constructor](#66-complete-example-with-constructor)

## [CHAPTER 7: VISIBILITY SECTIONS - ACCESS CONTROL](#chapter-7-visibility-sections--access-control)
  - [7.1 Encapsulation Concept](#71-encapsulation-concept)
  - [7.2 PUBLIC Section](#72-public-section)
  - [7.3 PROTECTED Section](#73-protected-section)
  - [7.4 PRIVATE Section](#74-private-section)
  - [7.5 Comparison Table](#75-comparison-table)
  - [7.6 Real-World Analogy](#76-real-world-analogy)
  - [7.7 Best Practices for Visibility](#77-best-practices-for-visibility)

## [CHAPTER 8: STATIC COMPONENTS - CLASS-LEVEL MEMBERS](#chapter-8-static-components--class-level-members)
  - [8.1 Instance vs Static](#81-instance-vs-static)
  - [8.2 Static Attributes (CLASS-DATA)](#82-static-attributes-class-data)
  - [8.3 Static Methods (CLASS-METHODS)](#83-static-methods-class-methods)
  - [8.4 Constants (CONSTANTS)](#84-constants-constants)
  - [8.5 When to Use Static Components](#85-when-to-use-static-components)
  - [8.6 Practical Examples](#86-practical-examples)
  - [8.7 Exercise 3](#87-exercise-3)

## [CHAPTER 9: INHERITANCE - CODE REUSABILITY](#chapter-9-inheritance--code-reusability)
  - [9.1 Understanding Inheritance](#91-understanding-inheritance)
  - [9.2 Parent and Child Classes](#92-parent-and-child-classes)
  - [9.3 Inheritance Syntax](#93-inheritance-syntax)
  - [9.4 Inheriting Attributes and Methods](#94-inheriting-attributes-and-methods)
  - [9.5 The SUPER Keyword](#95-the-super-keyword)
  - [9.6 Single vs Multiple Inheritance](#96-single-vs-multiple-inheritance)
  - [9.7 Real-World Example: Vehicle Hierarchy](#97-real-world-example-vehicle-hierarchy)
  - [9.8 Best Practices for Inheritance](#98-best-practices-for-inheritance)
  - [9.9 Exercise 4](#99-exercise-4)

## [CHAPTER 10: METHOD OVERRIDING - CUSTOMIZING BEHAVIOR](#chapter-10-method-overriding--customizing-behavior)
  - [10.1 Understanding Method Overriding](#101-understanding-method-overriding)
  - [10.2 REDEFINITION Keyword](#102-redefinition-keyword)
  - [10.3 Rules of Method Overriding](#103-rules-of-method-overriding)
  - [10.4 Polymorphism in Action](#104-polymorphism-in-action)
  - [10.5 Calling Parent Method](#105-calling-parent-method)
  - [10.6 Practical Examples](#106-practical-examples)
  - [10.7 Common Mistakes](#107-common-mistakes)
  - [10.8 Exercise 5](#108-exercise-5)

---

# CHAPTER 1: INTRODUCTION TO OBJECT ORIENTED PROGRAMMING

## 1.1 What is Object Oriented Programming?

Object Oriented Programming (OOP) is a programming paradigm that organizes code around "objects" rather than functions and logic. An object is a self-contained unit that contains both data (attributes) and behavior (methods).

### Real-World Analogy

Think of a **Car**:
- **Attributes (Data):** Color, Model, Speed, Fuel Level
- **Behavior (Methods):** Start(), Stop(), Accelerate(), Brake()

Every car object has these characteristics, but each car can have different values.

## 1.2 Why OOP in ABAP?

### Traditional Procedural ABAP Problems:

```abap
* Procedural Approach - Old Way
DATA: lv_matnr TYPE matnr,
      lv_maktx TYPE maktx,
      lv_price TYPE p DECIMALS 2.

PERFORM get_material_data USING lv_matnr CHANGING lv_maktx.
PERFORM calculate_price USING lv_matnr CHANGING lv_price.
PERFORM display_output USING lv_matnr lv_maktx lv_price.
```

**Problems with Procedural Approach:**
1. Data and functions are separate
2. Difficult to maintain and modify
3. Code duplication across programs
4. No encapsulation - data is exposed
5. Testing is complicated

### OOP Solution:

```abap
* OOP Approach - Modern Way
DATA: lo_material TYPE REF TO zcl_material.

CREATE OBJECT lo_material
  EXPORTING
    iv_matnr = '100001'.

lo_material->get_data( ).
lo_material->calculate_price( ).
lo_material->display( ).
```

**Benefits of OOP:**
1. Data and methods are bundled together
2. Easy to maintain and extend
3. Code reusability through inheritance
4. Encapsulation protects data
5. Easy unit testing

## 1.3 Four Pillars of OOP

```
┌─────────────────────────────────────────────────────────────┐
│                    FOUR PILLARS OF OOP                      │
├─────────────────┬─────────────────┬─────────────────────────┤
│  ENCAPSULATION  │   INHERITANCE   │     POLYMORPHISM        │
│                 │                 │                         │
│ Hiding internal │ Acquiring       │ Same interface,         │
│ details from    │ properties from │ different               │
│ outside world   │ parent class    │ implementations         │
├─────────────────┴─────────────────┴─────────────────────────┤
│                      ABSTRACTION                            │
│                                                             │
│     Showing only essential details, hiding complexity       │
└─────────────────────────────────────────────────────────────┘
```

## 1.4 OOP Terminology in ABAP

| Term | Description | ABAP Keyword |
|------|-------------|--------------|
| Class | Blueprint/Template for objects | CLASS...ENDCLASS |
| Object | Instance of a class | CREATE OBJECT |
| Attribute | Variables inside a class | DATA |
| Method | Functions inside a class | METHODS |
| Constructor | Special method for initialization | CONSTRUCTOR |
| Interface | Contract for classes | INTERFACE |
| Inheritance | Parent-child relationship | INHERITING FROM |

## 1.5 First OOP Program in ABAP

Let's create our first simple class:

```abap
*&---------------------------------------------------------------------*
*& Report Z_FIRST_OOP_PROGRAM
*&---------------------------------------------------------------------*
REPORT z_first_oop_program.

*----------------------------------------------------------------------*
*       CLASS lcl_hello DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_hello DEFINITION.
  PUBLIC SECTION.
    METHODS: say_hello.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_hello IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_hello IMPLEMENTATION.
  METHOD say_hello.
    WRITE: / 'Hello! Welcome to ABAP OOP World!'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_hello TYPE REF TO lcl_hello.
  
  CREATE OBJECT lo_hello.
  lo_hello->say_hello( ).
```

**Output:**
```
Hello! Welcome to ABAP OOP World!
```

### Code Explanation:

1. **CLASS lcl_hello DEFINITION** - Declares what the class contains
2. **PUBLIC SECTION** - Components accessible from outside
3. **METHODS: say_hello** - Declares a method
4. **CLASS lcl_hello IMPLEMENTATION** - Contains actual code
5. **DATA: lo_hello TYPE REF TO lcl_hello** - Reference variable
6. **CREATE OBJECT lo_hello** - Creates an instance
7. **lo_hello->say_hello( )** - Calls the method

---

# CHAPTER 2: CLASSES - THE BLUEPRINT OF OBJECTS

## 2.1 Understanding Classes

A class is like a blueprint or template. Just as an architect creates a blueprint before building a house, we create a class before creating objects.

```
┌─────────────────────────────────────────────────────────────┐
│                    CLASS STRUCTURE                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   CLASS <class_name> DEFINITION.                           │
│     PUBLIC SECTION.                                         │
│       " Public components                                   │
│     PROTECTED SECTION.                                      │
│       " Protected components                                │
│     PRIVATE SECTION.                                        │
│       " Private components                                  │
│   ENDCLASS.                                                 │
│                                                             │
│   CLASS <class_name> IMPLEMENTATION.                        │
│     " Method implementations                                │
│   ENDCLASS.                                                 │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 2.2 Types of Classes in ABAP

### 2.2.1 Local Classes

Local classes are defined within a program and can only be used within that program.

```abap
REPORT z_local_class_demo.

*----------------------------------------------------------------------*
*       CLASS lcl_calculator DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_calculator DEFINITION.
  PUBLIC SECTION.
    METHODS: add IMPORTING iv_num1 TYPE i
                           iv_num2 TYPE i
                 RETURNING VALUE(rv_result) TYPE i,
             
             subtract IMPORTING iv_num1 TYPE i
                                iv_num2 TYPE i
                      RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_calculator IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_calculator IMPLEMENTATION.
  METHOD add.
    rv_result = iv_num1 + iv_num2.
  ENDMETHOD.
  
  METHOD subtract.
    rv_result = iv_num1 - iv_num2.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_calc TYPE REF TO lcl_calculator,
        lv_sum TYPE i,
        lv_diff TYPE i.
  
  CREATE OBJECT lo_calc.
  
  lv_sum = lo_calc->add( iv_num1 = 100 iv_num2 = 50 ).
  lv_diff = lo_calc->subtract( iv_num1 = 100 iv_num2 = 50 ).
  
  WRITE: / 'Sum:', lv_sum,
         / 'Difference:', lv_diff.
```

**Output:**
```
Sum: 150
Difference: 50
```

### 2.2.2 Global Classes

Global classes are created in the Class Builder (Transaction SE24) and can be used across multiple programs.

**Steps to Create Global Class:**

1. Go to Transaction SE24
2. Enter class name (e.g., ZCL_MATERIAL)
3. Click Create
4. Choose Class Type: Regular Class
5. Add attributes and methods
6. Activate

```abap
*----------------------------------------------------------------------*
* Global Class: ZCL_EMPLOYEE (Created in SE24)
*----------------------------------------------------------------------*
* This class would be created in SE24, shown here for reference

CLASS zcl_employee DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_employee,
             emp_id   TYPE numc10,
             emp_name TYPE char50,
             salary   TYPE p LENGTH 8 DECIMALS 2,
           END OF ty_employee.
    
    METHODS: constructor IMPORTING iv_emp_id TYPE numc10,
             get_details RETURNING VALUE(rs_employee) TYPE ty_employee,
             set_salary IMPORTING iv_salary TYPE p.

  PRIVATE SECTION.
    DATA: ms_employee TYPE ty_employee.
ENDCLASS.

CLASS zcl_employee IMPLEMENTATION.
  METHOD constructor.
    ms_employee-emp_id = iv_emp_id.
    " Fetch employee details from database
    SELECT SINGLE * FROM zemployee 
      INTO CORRESPONDING FIELDS OF ms_employee
      WHERE emp_id = iv_emp_id.
  ENDMETHOD.
  
  METHOD get_details.
    rs_employee = ms_employee.
  ENDMETHOD.
  
  METHOD set_salary.
    ms_employee-salary = iv_salary.
  ENDMETHOD.
ENDCLASS.
```

## 2.3 Naming Conventions (Industry Standards)

| Type | Prefix | Example |
|------|--------|---------|
| Local Class | LCL_ | lcl_calculator |
| Global Class | ZCL_ or YCL_ | zcl_material |
| Interface | ZIF_ or LIF_ | zif_printable |
| Exception Class | ZCX_ | zcx_material_error |
| Test Class | LTC_ | ltc_test_material |

## 2.4 Class Attributes

Class attributes represent the data that objects can hold.

```abap
CLASS lcl_product DEFINITION.
  PUBLIC SECTION.
    DATA: mv_product_id TYPE char10,
          mv_name       TYPE char50.
    
  PRIVATE SECTION.
    DATA: mv_cost_price  TYPE p LENGTH 8 DECIMALS 2,
          mv_sale_price  TYPE p LENGTH 8 DECIMALS 2.
ENDCLASS.
```

### Types of Attributes:

```
┌─────────────────────────────────────────────────────────────┐
│                    TYPES OF ATTRIBUTES                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  1. INSTANCE ATTRIBUTES (DATA)                              │
│     - Each object has its own copy                          │
│     - Accessed using object reference                       │
│     - Example: DATA: mv_name TYPE string.                   │
│                                                             │
│  2. STATIC ATTRIBUTES (CLASS-DATA)                          │
│     - Shared by all objects                                 │
│     - Accessed using class name                             │
│     - Example: CLASS-DATA: mv_count TYPE i.                 │
│                                                             │
│  3. CONSTANTS (CONSTANTS)                                   │
│     - Fixed values that cannot change                       │
│     - Example: CONSTANTS: c_pi TYPE p VALUE '3.14'.         │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 2.5 Complete Example: Bank Account Class

```abap
REPORT z_bank_account_demo.

*----------------------------------------------------------------------*
*       CLASS lcl_bank_account DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_bank_account DEFINITION.
  PUBLIC SECTION.
    " Constants
    CONSTANTS: c_min_balance TYPE p LENGTH 8 DECIMALS 2 VALUE '1000.00'.
    
    " Instance attributes (visible)
    DATA: mv_account_no TYPE char12 READ-ONLY,
          mv_holder_name TYPE char50 READ-ONLY.
    
    " Class attribute (static)
    CLASS-DATA: mv_total_accounts TYPE i READ-ONLY.
    
    " Methods
    METHODS: constructor IMPORTING iv_acc_no TYPE char12
                                   iv_name   TYPE char50
                                   iv_initial_deposit TYPE p,
             deposit IMPORTING iv_amount TYPE p,
             withdraw IMPORTING iv_amount TYPE p
                      RETURNING VALUE(rv_success) TYPE abap_bool,
             get_balance RETURNING VALUE(rv_balance) TYPE p,
             display_info.
             
  PRIVATE SECTION.
    DATA: mv_balance TYPE p LENGTH 12 DECIMALS 2.
    
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_bank_account IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_bank_account IMPLEMENTATION.

  METHOD constructor.
    mv_account_no = iv_acc_no.
    mv_holder_name = iv_name.
    mv_balance = iv_initial_deposit.
    
    " Increment total account count
    ADD 1 TO mv_total_accounts.
    
    WRITE: / 'Account created successfully for:', mv_holder_name.
  ENDMETHOD.

  METHOD deposit.
    IF iv_amount > 0.
      mv_balance = mv_balance + iv_amount.
      WRITE: / 'Deposited:', iv_amount, 
             / 'New Balance:', mv_balance.
    ELSE.
      WRITE: / 'Error: Invalid deposit amount'.
    ENDIF.
  ENDMETHOD.

  METHOD withdraw.
    DATA: lv_remaining TYPE p LENGTH 12 DECIMALS 2.
    
    lv_remaining = mv_balance - iv_amount.
    
    IF lv_remaining >= c_min_balance.
      mv_balance = lv_remaining.
      rv_success = abap_true.
      WRITE: / 'Withdrawn:', iv_amount,
             / 'Remaining Balance:', mv_balance.
    ELSE.
      rv_success = abap_false.
      WRITE: / 'Error: Insufficient balance.',
             / 'Minimum balance of', c_min_balance, 'must be maintained.'.
    ENDIF.
  ENDMETHOD.

  METHOD get_balance.
    rv_balance = mv_balance.
  ENDMETHOD.

  METHOD display_info.
    WRITE: / '========================================',
           / 'Account Number:', mv_account_no,
           / 'Holder Name:', mv_holder_name,
           / 'Current Balance:', mv_balance,
           / '========================================'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_account1 TYPE REF TO lcl_bank_account,
        lo_account2 TYPE REF TO lcl_bank_account,
        lv_success TYPE abap_bool.

  WRITE: / 'Creating Bank Accounts...', /.
  
  " Create first account
  CREATE OBJECT lo_account1
    EXPORTING
      iv_acc_no = 'ACC001'
      iv_name   = 'John Smith'
      iv_initial_deposit = '5000.00'.
  
  SKIP.
  
  " Create second account
  CREATE OBJECT lo_account2
    EXPORTING
      iv_acc_no = 'ACC002'
      iv_name   = 'Jane Doe'
      iv_initial_deposit = '10000.00'.
  
  SKIP.
  WRITE: / 'Total Accounts Created:', lcl_bank_account=>mv_total_accounts.
  
  SKIP.
  WRITE: / '--- Operations on Account 1 ---'.
  lo_account1->deposit( iv_amount = '2000.00' ).
  SKIP.
  lv_success = lo_account1->withdraw( iv_amount = '1500.00' ).
  SKIP.
  
  WRITE: / '--- Attempting to withdraw more than allowed ---'.
  lv_success = lo_account1->withdraw( iv_amount = '5000.00' ).
  
  SKIP.
  WRITE: / '--- Account Information ---'.
  lo_account1->display_info( ).
  lo_account2->display_info( ).
```

**Output:**
```
Creating Bank Accounts...

Account created successfully for: John Smith

Account created successfully for: Jane Doe

Total Accounts Created: 2

--- Operations on Account 1 ---
Deposited: 2,000.00
New Balance: 7,000.00

Withdrawn: 1,500.00
Remaining Balance: 5,500.00

--- Attempting to withdraw more than allowed ---
Error: Insufficient balance.
Minimum balance of 1,000.00 must be maintained.

--- Account Information ---
========================================
Account Number: ACC001
Holder Name: John Smith
Current Balance: 5,500.00
========================================
========================================
Account Number: ACC002
Holder Name: Jane Doe
Current Balance: 10,000.00
========================================
```

## 2.6 Exercise 1: Create a Student Class

**Problem:** Create a class `lcl_student` with the following:

**Attributes:**
- Student ID (public, read-only)
- Student Name (public, read-only)
- Marks in 3 subjects (private)
- Total marks (private)
- Percentage (private)

**Methods:**
- Constructor (initialize student ID and name)
- set_marks (set marks for 3 subjects)
- calculate_percentage
- get_grade (return grade based on percentage)
- display_result

**Grade Criteria:**
- >= 90: A+
- >= 80: A
- >= 70: B
- >= 60: C
- >= 50: D
- < 50: F

### Solution:

```abap
REPORT z_student_exercise.

*----------------------------------------------------------------------*
*       CLASS lcl_student DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_student DEFINITION.
  PUBLIC SECTION.
    DATA: mv_student_id   TYPE numc10 READ-ONLY,
          mv_student_name TYPE char50 READ-ONLY.
    
    METHODS: constructor IMPORTING iv_id   TYPE numc10
                                   iv_name TYPE char50,
             set_marks IMPORTING iv_mark1 TYPE i
                                 iv_mark2 TYPE i
                                 iv_mark3 TYPE i,
             calculate_percentage RETURNING VALUE(rv_percentage) TYPE p,
             get_grade RETURNING VALUE(rv_grade) TYPE char2,
             display_result.
             
  PRIVATE SECTION.
    DATA: mv_mark1      TYPE i,
          mv_mark2      TYPE i,
          mv_mark3      TYPE i,
          mv_total      TYPE i,
          mv_percentage TYPE p LENGTH 5 DECIMALS 2.
          
    METHODS: calculate_total.
    
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_student IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_student IMPLEMENTATION.

  METHOD constructor.
    mv_student_id = iv_id.
    mv_student_name = iv_name.
  ENDMETHOD.

  METHOD set_marks.
    " Validate marks (0-100)
    IF iv_mark1 BETWEEN 0 AND 100 AND
       iv_mark2 BETWEEN 0 AND 100 AND
       iv_mark3 BETWEEN 0 AND 100.
      mv_mark1 = iv_mark1.
      mv_mark2 = iv_mark2.
      mv_mark3 = iv_mark3.
      me->calculate_total( ).
    ELSE.
      WRITE: / 'Error: Marks must be between 0 and 100'.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_total.
    mv_total = mv_mark1 + mv_mark2 + mv_mark3.
  ENDMETHOD.

  METHOD calculate_percentage.
    mv_percentage = ( mv_total / 3 ).
    rv_percentage = mv_percentage.
  ENDMETHOD.

  METHOD get_grade.
    " Ensure percentage is calculated
    IF mv_percentage IS INITIAL.
      me->calculate_percentage( ).
    ENDIF.
    
    IF mv_percentage >= 90.
      rv_grade = 'A+'.
    ELSEIF mv_percentage >= 80.
      rv_grade = 'A'.
    ELSEIF mv_percentage >= 70.
      rv_grade = 'B'.
    ELSEIF mv_percentage >= 60.
      rv_grade = 'C'.
    ELSEIF mv_percentage >= 50.
      rv_grade = 'D'.
    ELSE.
      rv_grade = 'F'.
    ENDIF.
  ENDMETHOD.

  METHOD display_result.
    DATA: lv_grade TYPE char2.
    
    me->calculate_percentage( ).
    lv_grade = me->get_grade( ).
    
    WRITE: / '╔══════════════════════════════════════╗',
           / '║         STUDENT RESULT CARD          ║',
           / '╠══════════════════════════════════════╣',
           / '║ Student ID  :', mv_student_id,
           / '║ Student Name:', mv_student_name,
           / '╠══════════════════════════════════════╣',
           / '║ Subject 1   :', mv_mark1,
           / '║ Subject 2   :', mv_mark2,
           / '║ Subject 3   :', mv_mark3,
           / '╠══════════════════════════════════════╣',
           / '║ Total Marks :', mv_total, '/ 300',
           / '║ Percentage  :', mv_percentage, '%',
           / '║ Grade       :', lv_grade,
           / '╚══════════════════════════════════════╝'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_student1 TYPE REF TO lcl_student,
        lo_student2 TYPE REF TO lcl_student.
  
  " Create students
  CREATE OBJECT lo_student1
    EXPORTING
      iv_id   = '1001'
      iv_name = 'Alice Johnson'.
  
  CREATE OBJECT lo_student2
    EXPORTING
      iv_id   = '1002'
      iv_name = 'Bob Williams'.
  
  " Set marks
  lo_student1->set_marks( iv_mark1 = 85 iv_mark2 = 92 iv_mark3 = 88 ).
  lo_student2->set_marks( iv_mark1 = 65 iv_mark2 = 70 iv_mark3 = 68 ).
  
  " Display results
  lo_student1->display_result( ).
  SKIP.
  lo_student2->display_result( ).
```

---

# CHAPTER 3: OBJECTS - BRINGING CLASSES TO LIFE

## 3.1 Understanding Objects

An object is an instance of a class. While a class is just a blueprint, an object is the actual entity created from that blueprint.

```
┌─────────────────────────────────────────────────────────────┐
│                  CLASS vs OBJECT                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   CLASS (Blueprint)              OBJECT (Instance)          │
│   ┌─────────────────┐           ┌─────────────────┐        │
│   │     CAR         │           │    my_car       │        │
│   │ ─────────────── │           │ ─────────────── │        │
│   │ - color         │  ──────►  │ - color = "Red" │        │
│   │ - model         │  CREATE   │ - model = "BMW" │        │
│   │ - speed         │  OBJECT   │ - speed = 0     │        │
│   │ ─────────────── │           │ ─────────────── │        │
│   │ + start()       │           │ + start()       │        │
│   │ + stop()        │           │ + stop()        │        │
│   └─────────────────┘           └─────────────────┘        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 3.2 Creating Objects in ABAP

There are multiple ways to create objects in ABAP:

### Method 1: Using CREATE OBJECT (Classic)

```abap
DATA: lo_car TYPE REF TO lcl_car.

CREATE OBJECT lo_car.
" or
CREATE OBJECT lo_car
  EXPORTING
    iv_color = 'Red'
    iv_model = 'BMW'.
```

### Method 2: Using NEW Operator (ABAP 7.40+)

```abap
DATA: lo_car TYPE REF TO lcl_car.

lo_car = NEW #( ).
" or
lo_car = NEW #( iv_color = 'Red' iv_model = 'BMW' ).
" or with inline declaration
DATA(lo_car2) = NEW lcl_car( iv_color = 'Blue' iv_model = 'Audi' ).
```

## 3.3 Reference Variables

A reference variable holds the memory address of an object, not the object itself.

```abap
REPORT z_reference_demo.

CLASS lcl_person DEFINITION.
  PUBLIC SECTION.
    DATA: mv_name TYPE string.
    METHODS: constructor IMPORTING iv_name TYPE string.
ENDCLASS.

CLASS lcl_person IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_person1 TYPE REF TO lcl_person,
        lo_person2 TYPE REF TO lcl_person.
  
  " Create object
  CREATE OBJECT lo_person1
    EXPORTING iv_name = 'John'.
  
  " lo_person2 points to the same object as lo_person1
  lo_person2 = lo_person1.
  
  WRITE: / 'Person 1 Name:', lo_person1->mv_name.
  WRITE: / 'Person 2 Name:', lo_person2->mv_name.
  
  " Change name through lo_person2
  lo_person2->mv_name = 'Jane'.
  
  WRITE: / 'After change:'.
  WRITE: / 'Person 1 Name:', lo_person1->mv_name.  " Also shows 'Jane'
  WRITE: / 'Person 2 Name:', lo_person2->mv_name.
```

**Output:**
```
Person 1 Name: John
Person 2 Name: John
After change:
Person 1 Name: Jane
Person 2 Name: Jane
```

## 3.4 Object Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│                   OBJECT LIFECYCLE                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   1. DECLARATION                                            │
│      DATA: lo_obj TYPE REF TO lcl_class.                   │
│      ↓                                                      │
│   2. INSTANTIATION                                          │
│      CREATE OBJECT lo_obj.                                  │
│      ↓                                                      │
│   3. USAGE                                                  │
│      lo_obj->method( ).                                     │
│      ↓                                                      │
│   4. DESTRUCTION                                            │
│      CLEAR lo_obj. (or goes out of scope)                  │
│      Garbage Collector removes object from memory           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 3.5 Multiple Objects from Same Class

```abap
REPORT z_multiple_objects.

CLASS lcl_employee DEFINITION.
  PUBLIC SECTION.
    DATA: mv_emp_id   TYPE numc6,
          mv_emp_name TYPE char40,
          mv_salary   TYPE p LENGTH 8 DECIMALS 2.
    
    CLASS-DATA: mv_emp_count TYPE i.
    
    METHODS: constructor IMPORTING iv_id     TYPE numc6
                                   iv_name   TYPE char40
                                   iv_salary TYPE p,
             display.
ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.
  METHOD constructor.
    mv_emp_id = iv_id.
    mv_emp_name = iv_name.
    mv_salary = iv_salary.
    ADD 1 TO mv_emp_count.
  ENDMETHOD.
  
  METHOD display.
    WRITE: / 'ID:', mv_emp_id, 'Name:', mv_emp_name, 'Salary:', mv_salary.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lt_employees TYPE TABLE OF REF TO lcl_employee,
        lo_emp       TYPE REF TO lcl_employee.
  
  " Create multiple employee objects
  CREATE OBJECT lo_emp EXPORTING iv_id = '001' iv_name = 'John' iv_salary = '50000'.
  APPEND lo_emp TO lt_employees.
  
  CREATE OBJECT lo_emp EXPORTING iv_id = '002' iv_name = 'Jane' iv_salary = '55000'.
  APPEND lo_emp TO lt_employees.
  
  CREATE OBJECT lo_emp EXPORTING iv_id = '003' iv_name = 'Bob' iv_salary = '48000'.
  APPEND lo_emp TO lt_employees.
  
  " Display all employees
  WRITE: / 'Total Employees:', lcl_employee=>mv_emp_count.
  SKIP.
  
  LOOP AT lt_employees INTO lo_emp.
    lo_emp->display( ).
  ENDLOOP.
```

## 3.6 Self-Reference: ME Keyword

The `ME` keyword refers to the current object instance.

```abap
CLASS lcl_counter DEFINITION.
  PUBLIC SECTION.
    DATA: mv_value TYPE i.
    
    METHODS: increment RETURNING VALUE(ro_self) TYPE REF TO lcl_counter,
             decrement RETURNING VALUE(ro_self) TYPE REF TO lcl_counter,
             get_value RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS lcl_counter IMPLEMENTATION.
  METHOD increment.
    mv_value = mv_value + 1.
    ro_self = me.  " Return reference to self for method chaining
  ENDMETHOD.
  
  METHOD decrement.
    mv_value = mv_value - 1.
    ro_self = me.
  ENDMETHOD.
  
  METHOD get_value.
    rv_value = mv_value.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_counter TYPE REF TO lcl_counter.
  CREATE OBJECT lo_counter.
  
  " Method chaining using ME
  DATA(lv_result) = lo_counter->increment( 
                             )->increment( 
                             )->increment( 
                             )->decrement( 
                             )->get_value( ).
  
  WRITE: / 'Counter Value:', lv_result.  " Output: 2
```

---

# CHAPTER 4: ATTRIBUTES - DATA OF A CLASS

## 4.1 Types of Attributes

```
┌─────────────────────────────────────────────────────────────┐
│                    ATTRIBUTE TYPES                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ INSTANCE ATTRIBUTES (DATA)                          │  │
│   │ - Unique to each object                             │  │
│   │ - Created when object is instantiated               │  │
│   │ - Accessed via object reference: lo_obj->attribute  │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ STATIC ATTRIBUTES (CLASS-DATA)                      │  │
│   │ - Shared by all objects of the class                │  │
│   │ - Exists even without object creation               │  │
│   │ - Accessed via class name: class=>attribute         │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ CONSTANTS (CONSTANTS)                               │  │
│   │ - Value cannot be changed                           │  │
│   │ - Must be initialized at declaration                │  │
│   │ - Accessed via class name: class=>constant          │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 4.2 Complete Attributes Example

```abap
REPORT z_attributes_demo.

CLASS lcl_product DEFINITION.
  PUBLIC SECTION.
    " Constants
    CONSTANTS: c_tax_rate TYPE p LENGTH 3 DECIMALS 2 VALUE '18.00',
               c_max_quantity TYPE i VALUE 1000.
    
    " Static attribute
    CLASS-DATA: mv_product_count TYPE i.
    
    " Instance attributes
    DATA: mv_product_id   TYPE char10 READ-ONLY,
          mv_product_name TYPE char50,
          mv_quantity     TYPE i.
    
    METHODS: constructor IMPORTING iv_id   TYPE char10
                                   iv_name TYPE char50,
             set_quantity IMPORTING iv_qty TYPE i,
             get_price_with_tax IMPORTING iv_base_price TYPE p
                                RETURNING VALUE(rv_final_price) TYPE p,
             display.
    
    " Static method
    CLASS-METHODS: get_product_count RETURNING VALUE(rv_count) TYPE i.
    
  PRIVATE SECTION.
    DATA: mv_created_on TYPE datum.
    
ENDCLASS.

CLASS lcl_product IMPLEMENTATION.
  METHOD constructor.
    mv_product_id = iv_id.
    mv_product_name = iv_name.
    mv_created_on = sy-datum.
    ADD 1 TO mv_product_count.
  ENDMETHOD.
  
  METHOD set_quantity.
    IF iv_qty <= c_max_quantity.
      mv_quantity = iv_qty.
    ELSE.
      WRITE: / 'Error: Quantity cannot exceed', c_max_quantity.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_price_with_tax.
    DATA: lv_tax TYPE p LENGTH 10 DECIMALS 2.
    lv_tax = iv_base_price * c_tax_rate / 100.
    rv_final_price = iv_base_price + lv_tax.
  ENDMETHOD.
  
  METHOD display.
    WRITE: / 'Product ID:', mv_product_id,
           / 'Product Name:', mv_product_name,
           / 'Quantity:', mv_quantity,
           / 'Created On:', mv_created_on.
  ENDMETHOD.
  
  METHOD get_product_count.
    rv_count = mv_product_count.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_prod1 TYPE REF TO lcl_product,
        lo_prod2 TYPE REF TO lcl_product.
  
  " Access constant without creating object
  WRITE: / 'Tax Rate:', lcl_product=>c_tax_rate, '%'.
  WRITE: / 'Max Quantity:', lcl_product=>c_max_quantity.
  
  SKIP.
  
  " Create products
  CREATE OBJECT lo_prod1 EXPORTING iv_id = 'P001' iv_name = 'Laptop'.
  lo_prod1->set_quantity( 50 ).
  
  CREATE OBJECT lo_prod2 EXPORTING iv_id = 'P002' iv_name = 'Mouse'.
  lo_prod2->set_quantity( 200 ).
  
  " Try to exceed max quantity
  lo_prod2->set_quantity( 1500 ).
  
  SKIP.
  
  " Access static attribute
  WRITE: / 'Total Products:', lcl_product=>get_product_count( ).
  
  SKIP.
  
  " Calculate price with tax
  DATA(lv_price) = lo_prod1->get_price_with_tax( iv_base_price = '1000.00' ).
  WRITE: / 'Price with Tax:', lv_price.
  
  SKIP.
  lo_prod1->display( ).
```

## 4.3 READ-ONLY Attribute

```abap
CLASS lcl_immutable DEFINITION.
  PUBLIC SECTION.
    " READ-ONLY: Can be read from outside but only modified within the class
    DATA: mv_id TYPE i READ-ONLY.
    
    METHODS: constructor IMPORTING iv_id TYPE i,
             update_id IMPORTING iv_new_id TYPE i.
ENDCLASS.

CLASS lcl_immutable IMPLEMENTATION.
  METHOD constructor.
    mv_id = iv_id.
  ENDMETHOD.
  
  METHOD update_id.
    " This works because we're inside the class
    mv_id = iv_new_id.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_obj TYPE REF TO lcl_immutable.
  CREATE OBJECT lo_obj EXPORTING iv_id = 100.
  
  " Reading works
  WRITE: / 'ID:', lo_obj->mv_id.
  
  " This would cause syntax error:
  " lo_obj->mv_id = 200.  " Error: READ-ONLY
  
  " But this works (updating through method)
  lo_obj->update_id( 200 ).
  WRITE: / 'Updated ID:', lo_obj->mv_id.
```

---

# CHAPTER 5: METHODS - BEHAVIOR OF A CLASS

## 5.1 Understanding Methods

Methods define the behavior of objects. They are like functions that belong to a class.

```
┌─────────────────────────────────────────────────────────────┐
│                    METHOD STRUCTURE                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   METHODS method_name                                       │
│     IMPORTING parameter1 TYPE type1                         │
│               parameter2 TYPE type2 OPTIONAL                │
│               parameter3 TYPE type3 DEFAULT value           │
│     EXPORTING parameter4 TYPE type4                         │
│     CHANGING  parameter5 TYPE type5                         │
│     RETURNING VALUE(result) TYPE type6                      │
│     RAISING   exception1 exception2.                        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 5.2 Method Parameters

### Parameter Types:

| Type | Description | Passing |
|------|-------------|---------|
| IMPORTING | Input parameters | Pass by value (default) or reference |
| EXPORTING | Output parameters | Pass by reference |
| CHANGING | Input/Output parameters | Pass by reference |
| RETURNING | Functional method return | Pass by value |

### Parameter Passing:

```abap
CLASS lcl_parameter_demo DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " IMPORTING - Input only
      method_import IMPORTING iv_value TYPE i,
      
      " EXPORTING - Output only
      method_export EXPORTING ev_value TYPE i,
      
      " CHANGING - Input and Output
      method_change CHANGING cv_value TYPE i,
      
      " RETURNING - Functional style
      method_return IMPORTING iv_value TYPE i
                    RETURNING VALUE(rv_result) TYPE i,
      
      " Mixed parameters
      method_mixed IMPORTING iv_num1 TYPE i
                             iv_num2 TYPE i
                   EXPORTING ev_sum  TYPE i
                   CHANGING  cv_count TYPE i
                   RETURNING VALUE(rv_product) TYPE i.
ENDCLASS.

CLASS lcl_parameter_demo IMPLEMENTATION.
  METHOD method_import.
    WRITE: / 'Received value:', iv_value.
  ENDMETHOD.
  
  METHOD method_export.
    ev_value = 100.
  ENDMETHOD.
  
  METHOD method_change.
    cv_value = cv_value * 2.
  ENDMETHOD.
  
  METHOD method_return.
    rv_result = iv_value * iv_value.
  ENDMETHOD.
  
  METHOD method_mixed.
    ev_sum = iv_num1 + iv_num2.
    cv_count = cv_count + 1.
    rv_product = iv_num1 * iv_num2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_obj    TYPE REF TO lcl_parameter_demo,
        lv_value  TYPE i,
        lv_sum    TYPE i,
        lv_count  TYPE i VALUE 5,
        lv_result TYPE i.
  
  CREATE OBJECT lo_obj.
  
  " IMPORTING
  lo_obj->method_import( iv_value = 50 ).
  
  " EXPORTING
  lo_obj->method_export( IMPORTING ev_value = lv_value ).
  WRITE: / 'Exported value:', lv_value.
  
  " CHANGING
  lv_value = 25.
  WRITE: / 'Before CHANGING:', lv_value.
  lo_obj->method_change( CHANGING cv_value = lv_value ).
  WRITE: / 'After CHANGING:', lv_value.
  
  " RETURNING
  lv_result = lo_obj->method_return( iv_value = 5 ).
  WRITE: / 'Returned value:', lv_result.
  
  " Mixed
  lv_result = lo_obj->method_mixed(
                EXPORTING iv_num1 = 10
                          iv_num2 = 20
                IMPORTING ev_sum = lv_sum
                CHANGING cv_count = lv_count ).
  WRITE: / 'Sum:', lv_sum, 'Count:', lv_count, 'Product:', lv_result.
```

## 5.3 Instance Methods vs Static Methods

```abap
REPORT z_method_types.

CLASS lcl_math_utils DEFINITION.
  PUBLIC SECTION.
    " Static method - Can be called without object
    CLASS-METHODS:
      add IMPORTING iv_a TYPE i iv_b TYPE i
          RETURNING VALUE(rv_sum) TYPE i,
      
      multiply IMPORTING iv_a TYPE i iv_b TYPE i
               RETURNING VALUE(rv_product) TYPE i.
    
    " Instance method - Requires object
    METHODS:
      set_base IMPORTING iv_base TYPE i,
      get_power IMPORTING iv_exponent TYPE i
                RETURNING VALUE(rv_result) TYPE i.
    
  PRIVATE SECTION.
    DATA: mv_base TYPE i.
    
ENDCLASS.

CLASS lcl_math_utils IMPLEMENTATION.
  METHOD add.
    rv_sum = iv_a + iv_b.
  ENDMETHOD.
  
  METHOD multiply.
    rv_product = iv_a * iv_b.
  ENDMETHOD.
  
  METHOD set_base.
    mv_base = iv_base.
  ENDMETHOD.
  
  METHOD get_power.
    rv_result = mv_base ** iv_exponent.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_math TYPE REF TO lcl_math_utils.
  
  " Static methods - No object needed
  WRITE: / 'Static Methods (No object needed):'.
  WRITE: / '5 + 3 =', lcl_math_utils=>add( iv_a = 5 iv_b = 3 ).
  WRITE: / '5 * 3 =', lcl_math_utils=>multiply( iv_a = 5 iv_b = 3 ).
  
  SKIP.
  
  " Instance methods - Object needed
  WRITE: / 'Instance Methods (Object needed):'.
  CREATE OBJECT lo_math.
  lo_math->set_base( 2 ).
  WRITE: / '2^10 =', lo_math->get_power( 10 ).
```

## 5.4 Optional and Default Parameters

```abap
CLASS lcl_greeting DEFINITION.
  PUBLIC SECTION.
    METHODS:
      greet IMPORTING iv_name     TYPE string
                      iv_greeting TYPE string DEFAULT 'Hello'
                      iv_formal   TYPE abap_bool OPTIONAL
            RETURNING VALUE(rv_message) TYPE string.
ENDCLASS.

CLASS lcl_greeting IMPLEMENTATION.
  METHOD greet.
    IF iv_formal = abap_true.
      rv_message = |{ iv_greeting }, Mr./Ms. { iv_name }. How may I assist you?|.
    ELSE.
      rv_message = |{ iv_greeting }, { iv_name }!|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_greet TYPE REF TO lcl_greeting.
  CREATE OBJECT lo_greet.
  
  " Using default greeting
  WRITE: / lo_greet->greet( iv_name = 'John' ).
  
  " Custom greeting
  WRITE: / lo_greet->greet( iv_name = 'Jane' iv_greeting = 'Welcome' ).
  
  " Formal greeting
  WRITE: / lo_greet->greet( iv_name = 'Smith' iv_formal = abap_true ).
```

**Output:**
```
Hello, John!
Welcome, Jane!
Hello, Mr./Ms. Smith. How may I assist you?
```

## 5.5 Functional Method Calls (ABAP 7.40+)

```abap
CLASS lcl_string_utils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      to_upper IMPORTING iv_text TYPE string
               RETURNING VALUE(rv_result) TYPE string,
      
      to_lower IMPORTING iv_text TYPE string
               RETURNING VALUE(rv_result) TYPE string,
      
      reverse IMPORTING iv_text TYPE string
              RETURNING VALUE(rv_result) TYPE string,
      
      word_count IMPORTING iv_text TYPE string
                 RETURNING VALUE(rv_count) TYPE i.
ENDCLASS.

CLASS lcl_string_utils IMPLEMENTATION.
  METHOD to_upper.
    rv_result = to_upper( iv_text ).
  ENDMETHOD.
  
  METHOD to_lower.
    rv_result = to_lower( iv_text ).
  ENDMETHOD.
  
  METHOD reverse.
    rv_result = reverse( iv_text ).
  ENDMETHOD.
  
  METHOD word_count.
    DATA: lt_words TYPE TABLE OF string.
    SPLIT iv_text AT space INTO TABLE lt_words.
    rv_count = lines( lt_words ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lv_text) = |Hello World from ABAP|.
  
  " Functional method calls
  WRITE: / 'Original:', lv_text.
  WRITE: / 'Upper:', lcl_string_utils=>to_upper( lv_text ).
  WRITE: / 'Lower:', lcl_string_utils=>to_lower( lv_text ).
  WRITE: / 'Reversed:', lcl_string_utils=>reverse( lv_text ).
  WRITE: / 'Word Count:', lcl_string_utils=>word_count( lv_text ).
  
  " Method chaining
  WRITE: / 'Chained:', lcl_string_utils=>reverse( 
                         lcl_string_utils=>to_upper( lv_text ) ).
```

---

# CHAPTER 6: CONSTRUCTORS - OBJECT INITIALIZATION

## 6.1 Understanding Constructors

A constructor is a special method that is automatically called when an object is created. It's used to initialize the object's state.

```
┌─────────────────────────────────────────────────────────────┐
│                    CONSTRUCTOR TYPES                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   1. INSTANCE CONSTRUCTOR (CONSTRUCTOR)                     │
│      - Called when object is created                        │
│      - Can have IMPORTING parameters                        │
│      - Can raise exceptions                                 │
│      - Each object gets its own call                        │
│                                                             │
│   2. STATIC CONSTRUCTOR (CLASS_CONSTRUCTOR)                 │
│      - Called only once, before first access to class       │
│      - No parameters                                        │
│      - Cannot raise exceptions                              │
│      - Used to initialize static attributes                 │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 6.2 Instance Constructor

```abap
REPORT z_constructor_demo.

CLASS lcl_employee DEFINITION.
  PUBLIC SECTION.
    DATA: mv_emp_id   TYPE numc6 READ-ONLY,
          mv_emp_name TYPE char50 READ-ONLY,
          mv_dept     TYPE char20 READ-ONLY,
          mv_salary   TYPE p LENGTH 8 DECIMALS 2 READ-ONLY.
    
    " Constructor with parameters
    METHODS: constructor IMPORTING iv_emp_id TYPE numc6
                                   iv_name   TYPE char50
                                   iv_dept   TYPE char20 DEFAULT 'General'
                                   iv_salary TYPE p OPTIONAL.
    
    METHODS: display.
    
  PRIVATE SECTION.
    DATA: mv_created_at TYPE timestamp.
    
ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.
  METHOD constructor.
    " Initialize attributes
    mv_emp_id = iv_emp_id.
    mv_emp_name = iv_name.
    mv_dept = iv_dept.
    
    " Handle optional parameter
    IF iv_salary IS SUPPLIED.
      mv_salary = iv_salary.
    ELSE.
      mv_salary = '30000.00'.  " Default salary
    ENDIF.
    
    " Set timestamp
    GET TIME STAMP FIELD mv_created_at.
    
    WRITE: / 'Employee object created:', mv_emp_name.
  ENDMETHOD.
  
  METHOD display.
    WRITE: / 'Employee ID:', mv_emp_id,
           / 'Name:', mv_emp_name,
           / 'Department:', mv_dept,
           / 'Salary:', mv_salary.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_emp1 TYPE REF TO lcl_employee,
        lo_emp2 TYPE REF TO lcl_employee.
  
  " Create with all parameters
  CREATE OBJECT lo_emp1
    EXPORTING
      iv_emp_id = '000001'
      iv_name   = 'John Doe'
      iv_dept   = 'IT'
      iv_salary = '75000.00'.
  
  " Create with default department and salary
  CREATE OBJECT lo_emp2
    EXPORTING
      iv_emp_id = '000002'
      iv_name   = 'Jane Smith'.
  
  SKIP.
  WRITE: / '--- Employee 1 ---'.
  lo_emp1->display( ).
  
  SKIP.
  WRITE: / '--- Employee 2 ---'.
  lo_emp2->display( ).
```

## 6.3 Static Constructor

```abap
REPORT z_static_constructor.

CLASS lcl_database DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: mv_connection_string TYPE string READ-ONLY,
                mv_is_connected      TYPE abap_bool READ-ONLY.
    
    " Static constructor
    CLASS-METHODS: class_constructor.
    
    " Instance constructor
    METHODS: constructor IMPORTING iv_query TYPE string.
    
    METHODS: execute RETURNING VALUE(rv_result) TYPE string.
    
  PRIVATE SECTION.
    DATA: mv_query TYPE string.
    
ENDCLASS.

CLASS lcl_database IMPLEMENTATION.
  METHOD class_constructor.
    " Called only once when class is first accessed
    WRITE: / 'Static Constructor: Initializing database connection...'.
    
    mv_connection_string = 'SERVER=sap;DB=production;'.
    mv_is_connected = abap_true.
    
    WRITE: / 'Static Constructor: Connection established.'.
  ENDMETHOD.
  
  METHOD constructor.
    WRITE: / 'Instance Constructor: Creating query object...'.
    mv_query = iv_query.
  ENDMETHOD.
  
  METHOD execute.
    IF mv_is_connected = abap_true.
      rv_result = |Executed: { mv_query }|.
    ELSE.
      rv_result = 'Error: Not connected'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_query1 TYPE REF TO lcl_database,
        lo_query2 TYPE REF TO lcl_database.
  
  WRITE: / 'Accessing class for the first time...'.
  SKIP.
  
  " First access - static constructor will run
  CREATE OBJECT lo_query1
    EXPORTING iv_query = 'SELECT * FROM MARA'.
  
  SKIP.
  WRITE: / lo_query1->execute( ).
  
  SKIP.
  WRITE: / 'Creating second object...'.
  " Static constructor will NOT run again
  CREATE OBJECT lo_query2
    EXPORTING iv_query = 'SELECT * FROM MAKT'.
  
  SKIP.
  WRITE: / lo_query2->execute( ).
```

**Output:**
```
Accessing class for the first time...

Static Constructor: Initializing database connection...
Static Constructor: Connection established.
Instance Constructor: Creating query object...

Executed: SELECT * FROM MARA

Creating second object...
Instance Constructor: Creating query object...

Executed: SELECT * FROM MAKT
```

## 6.4 Constructor with Exception Handling

```abap
REPORT z_constructor_exception.

" Custom exception class
CLASS lcx_invalid_data DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA: mv_message TYPE string.
    METHODS: constructor IMPORTING iv_message TYPE string OPTIONAL
                                   previous   TYPE REF TO cx_root OPTIONAL.
    METHODS: get_text REDEFINITION.
ENDCLASS.

CLASS lcx_invalid_data IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    mv_message = iv_message.
  ENDMETHOD.
  
  METHOD get_text.
    result = mv_message.
  ENDMETHOD.
ENDCLASS.

" Main class with constructor that raises exception
CLASS lcl_order DEFINITION.
  PUBLIC SECTION.
    DATA: mv_order_id TYPE numc10 READ-ONLY,
          mv_quantity TYPE i READ-ONLY,
          mv_amount   TYPE p LENGTH 10 DECIMALS 2 READ-ONLY.
    
    METHODS: constructor IMPORTING iv_order_id TYPE numc10
                                   iv_quantity TYPE i
                                   iv_amount   TYPE p
                         RAISING   lcx_invalid_data.
    
    METHODS: display.
    
ENDCLASS.

CLASS lcl_order IMPLEMENTATION.
  METHOD constructor.
    " Validate order ID
    IF iv_order_id IS INITIAL.
      RAISE EXCEPTION TYPE lcx_invalid_data
        EXPORTING
          iv_message = 'Order ID cannot be empty'.
    ENDIF.
    
    " Validate quantity
    IF iv_quantity <= 0.
      RAISE EXCEPTION TYPE lcx_invalid_data
        EXPORTING
          iv_message = 'Quantity must be greater than zero'.
    ENDIF.
    
    " Validate amount
    IF iv_amount <= 0.
      RAISE EXCEPTION TYPE lcx_invalid_data
        EXPORTING
          iv_message = 'Amount must be greater than zero'.
    ENDIF.
    
    " If all validations pass
    mv_order_id = iv_order_id.
    mv_quantity = iv_quantity.
    mv_amount = iv_amount.
    
    WRITE: / 'Order created successfully'.
  ENDMETHOD.
  
  METHOD display.
    WRITE: / 'Order ID:', mv_order_id,
           / 'Quantity:', mv_quantity,
           / 'Amount:', mv_amount.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_order TYPE REF TO lcl_order,
        lo_error TYPE REF TO lcx_invalid_data.
  
  " Try to create valid order
  WRITE: / '--- Creating Valid Order ---'.
  TRY.
      CREATE OBJECT lo_order
        EXPORTING
          iv_order_id = '1000000001'
          iv_quantity = 10
          iv_amount   = '999.99'.
      
      lo_order->display( ).
    CATCH lcx_invalid_data INTO lo_error.
      WRITE: / 'Error:', lo_error->get_text( ).
  ENDTRY.
  
  SKIP.
  
  " Try to create order with invalid quantity
  WRITE: / '--- Creating Order with Invalid Quantity ---'.
  TRY.
      CREATE OBJECT lo_order
        EXPORTING
          iv_order_id = '1000000002'
          iv_quantity = 0
          iv_amount   = '500.00'.
    CATCH lcx_invalid_data INTO lo_error.
      WRITE: / 'Error:', lo_error->get_text( ).
  ENDTRY.
  
  SKIP.
  
  " Try to create order with empty ID
  WRITE: / '--- Creating Order with Empty ID ---'.
  TRY.
      CREATE OBJECT lo_order
        EXPORTING
          iv_order_id = ''
          iv_quantity = 5
          iv_amount   = '250.00'.
    CATCH lcx_invalid_data INTO lo_error.
      WRITE: / 'Error:', lo_error->get_text( ).
  ENDTRY.
```

---

# CHAPTER 7: VISIBILITY SECTIONS - ACCESS CONTROL

## 7.1 Understanding Visibility

Visibility determines who can access the components of a class.

```
┌─────────────────────────────────────────────────────────────┐
│                   VISIBILITY SECTIONS                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ PUBLIC SECTION                                      │  │
│   │ - Accessible from anywhere                          │  │
│   │ - External programs can access                      │  │
│   │ - Child classes can access                          │  │
│   │ - Same class can access                             │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ PROTECTED SECTION                                   │  │
│   │ - NOT accessible from outside                       │  │
│   │ - Child classes CAN access                          │  │
│   │ - Same class can access                             │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
│   ┌─────────────────────────────────────────────────────┐  │
│   │ PRIVATE SECTION                                     │  │
│   │ - Only same class can access                        │  │
│   │ - Child classes CANNOT access                       │  │
│   │ - Hidden from outside world                         │  │
│   └─────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 7.2 Visibility Example

```abap
REPORT z_visibility_demo.

*----------------------------------------------------------------------*
*       CLASS lcl_person DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_person DEFINITION.
  PUBLIC SECTION.
    DATA: mv_name TYPE string.
    METHODS: constructor IMPORTING iv_name TYPE string
                                   iv_age  TYPE i
                                   iv_ssn  TYPE char11,
             get_age RETURNING VALUE(rv_age) TYPE i,
             display_public_info.
    
  PROTECTED SECTION.
    DATA: mv_age TYPE i.
    METHODS: validate_age RETURNING VALUE(rv_valid) TYPE abap_bool.
    
  PRIVATE SECTION.
    DATA: mv_ssn TYPE char11.  " Social Security Number - sensitive data
    METHODS: encrypt_ssn RETURNING VALUE(rv_encrypted) TYPE string.
    
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_person IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_person IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
    mv_age = iv_age.
    mv_ssn = iv_ssn.
  ENDMETHOD.
  
  METHOD get_age.
    rv_age = mv_age.
  ENDMETHOD.
  
  METHOD validate_age.
    rv_valid = xsdbool( mv_age >= 0 AND mv_age <= 150 ).
  ENDMETHOD.
  
  METHOD encrypt_ssn.
    " Simple encryption for demo - hide middle digits
    rv_encrypted = |***-**-{ mv_ssn+7(4) }|.
  ENDMETHOD.
  
  METHOD display_public_info.
    WRITE: / 'Name:', mv_name,
           / 'Age:', mv_age,
           / 'SSN:', me->encrypt_ssn( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_employee DEFINITION (Child Class)
*----------------------------------------------------------------------*
CLASS lcl_employee DEFINITION INHERITING FROM lcl_person.
  PUBLIC SECTION.
    DATA: mv_emp_id TYPE numc6.
    
    METHODS: constructor IMPORTING iv_name   TYPE string
                                   iv_age    TYPE i
                                   iv_ssn    TYPE char11
                                   iv_emp_id TYPE numc6,
             is_valid_employee RETURNING VALUE(rv_valid) TYPE abap_bool,
             display_employee_info.
ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.
  METHOD constructor.
    " Call parent constructor
    super->constructor( iv_name = iv_name
                        iv_age  = iv_age
                        iv_ssn  = iv_ssn ).
    mv_emp_id = iv_emp_id.
  ENDMETHOD.
  
  METHOD is_valid_employee.
    " Can access PROTECTED method from parent
    rv_valid = me->validate_age( ).
    
    " Can access PROTECTED attribute from parent
    IF mv_age < 18.
      rv_valid = abap_false.
    ENDIF.
    
    " CANNOT access mv_ssn (PRIVATE in parent)
    " mv_ssn = '123'.  " This would cause syntax error
  ENDMETHOD.
  
  METHOD display_employee_info.
    WRITE: / 'Employee ID:', mv_emp_id,
           / 'Name:', mv_name,        " PUBLIC - accessible
           / 'Age:', mv_age.          " PROTECTED - accessible in child
    " Cannot access mv_ssn directly - PRIVATE in parent
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_person   TYPE REF TO lcl_person,
        lo_employee TYPE REF TO lcl_employee.
  
  CREATE OBJECT lo_person
    EXPORTING
      iv_name = 'John Doe'
      iv_age  = 30
      iv_ssn  = '123-45-6789'.
  
  " Accessing from outside the class:
  WRITE: / '--- Accessing from outside ---'.
  
  " PUBLIC - Works
  WRITE: / 'Name (PUBLIC):', lo_person->mv_name.
  
  " PROTECTED - Would cause syntax error
  " WRITE: / 'Age:', lo_person->mv_age.
  
  " PRIVATE - Would cause syntax error
  " WRITE: / 'SSN:', lo_person->mv_ssn.
  
  " But we can get age through public method
  WRITE: / 'Age (via method):', lo_person->get_age( ).
  
  SKIP.
  WRITE: / '--- Person Public Info ---'.
  lo_person->display_public_info( ).
  
  SKIP.
  
  " Create employee
  CREATE OBJECT lo_employee
    EXPORTING
      iv_name   = 'Jane Smith'
      iv_age    = 25
      iv_ssn    = '987-65-4321'
      iv_emp_id = '000100'.
  
  WRITE: / '--- Employee Info ---'.
  lo_employee->display_employee_info( ).
  
  SKIP.
  WRITE: / 'Is Valid Employee:', lo_employee->is_valid_employee( ).
```

## 7.3 Visibility Access Matrix

| Component Location | PUBLIC | PROTECTED | PRIVATE |
|-------------------|--------|-----------|---------|
| Same Class | ✅ | ✅ | ✅ |
| Child Class | ✅ | ✅ | ❌ |
| External Access | ✅ | ❌ | ❌ |

## 7.4 Friends - Breaking Visibility Rules

Sometimes you need one class to access private/protected members of another class. This is done using FRIENDS.

```abap
REPORT z_friends_demo.

CLASS lcl_wallet DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_bank DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_bank DEFINITION FRIENDS lcl_wallet.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_account TYPE char10,
             get_balance RETURNING VALUE(rv_balance) TYPE p.
    
  PRIVATE SECTION.
    DATA: mv_account TYPE char10,
          mv_balance TYPE p LENGTH 10 DECIMALS 2 VALUE '10000.00'.
    
ENDCLASS.

CLASS lcl_bank IMPLEMENTATION.
  METHOD constructor.
    mv_account = iv_account.
  ENDMETHOD.
  
  METHOD get_balance.
    rv_balance = mv_balance.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_wallet DEFINITION (Friend of lcl_bank)
*----------------------------------------------------------------------*
CLASS lcl_wallet DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_bank TYPE REF TO lcl_bank,
             withdraw IMPORTING iv_amount TYPE p
                      RETURNING VALUE(rv_success) TYPE abap_bool,
             deposit IMPORTING iv_amount TYPE p.
    
  PRIVATE SECTION.
    DATA: mo_bank TYPE REF TO lcl_bank.
    
ENDCLASS.

CLASS lcl_wallet IMPLEMENTATION.
  METHOD constructor.
    mo_bank = io_bank.
  ENDMETHOD.
  
  METHOD withdraw.
    " As a FRIEND, lcl_wallet can access PRIVATE members of lcl_bank
    IF mo_bank->mv_balance >= iv_amount.
      mo_bank->mv_balance = mo_bank->mv_balance - iv_amount.
      rv_success = abap_true.
      WRITE: / 'Withdrawn:', iv_amount.
    ELSE.
      rv_success = abap_false.
      WRITE: / 'Insufficient balance'.
    ENDIF.
  ENDMETHOD.
  
  METHOD deposit.
    " Directly accessing private attribute
    mo_bank->mv_balance = mo_bank->mv_balance + iv_amount.
    WRITE: / 'Deposited:', iv_amount.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_bank   TYPE REF TO lcl_bank,
        lo_wallet TYPE REF TO lcl_wallet.
  
  CREATE OBJECT lo_bank EXPORTING iv_account = 'ACC001'.
  CREATE OBJECT lo_wallet EXPORTING io_bank = lo_bank.
  
  WRITE: / 'Initial Balance:', lo_bank->get_balance( ).
  
  lo_wallet->deposit( '5000.00' ).
  WRITE: / 'After Deposit:', lo_bank->get_balance( ).
  
  lo_wallet->withdraw( '3000.00' ).
  WRITE: / 'After Withdrawal:', lo_bank->get_balance( ).
```

---

# CHAPTER 8: STATIC COMPONENTS - CLASS-LEVEL MEMBERS

## 8.1 Understanding Static Components

Static components belong to the class itself, not to individual objects.

```
┌─────────────────────────────────────────────────────────────┐
│                    STATIC vs INSTANCE                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   INSTANCE COMPONENTS          STATIC COMPONENTS            │
│   ─────────────────           ──────────────────            │
│   - DATA                      - CLASS-DATA                  │
│   - METHODS                   - CLASS-METHODS               │
│   - EVENTS                    - CLASS-EVENTS                │
│                                                             │
│   Object 1  Object 2          ┌─────────────────┐           │
│   ┌──────┐  ┌──────┐          │    CLASS        │           │
│   │data_a│  │data_a│          │  ┌───────────┐  │           │
│   │data_b│  │data_b│          │  │ static_x  │  │           │
│   └──────┘  └──────┘          │  │ static_y  │  │           │
│      ↑         ↑              │  └───────────┘  │           │
│      │         │              │  Shared by all  │           │
│   Each object has             │  objects        │           │
│   its own copy                └─────────────────┘           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 8.2 Static Attributes and Methods

```abap
REPORT z_static_demo.

CLASS lcl_counter DEFINITION.
  PUBLIC SECTION.
    " Static attribute - shared by all objects
    CLASS-DATA: mv_total_count TYPE i.
    
    " Instance attribute - unique to each object
    DATA: mv_instance_id TYPE i READ-ONLY.
    
    " Static methods
    CLASS-METHODS: 
      get_total_count RETURNING VALUE(rv_count) TYPE i,
      reset_count,
      class_constructor.
    
    " Instance methods
    METHODS:
      constructor,
      get_id RETURNING VALUE(rv_id) TYPE i.
    
ENDCLASS.

CLASS lcl_counter IMPLEMENTATION.
  METHOD class_constructor.
    WRITE: / 'Class constructor called - initializing...'.
    mv_total_count = 0.
  ENDMETHOD.
  
  METHOD constructor.
    ADD 1 TO mv_total_count.
    mv_instance_id = mv_total_count.
    WRITE: / 'Instance created with ID:', mv_instance_id.
  ENDMETHOD.
  
  METHOD get_total_count.
    rv_count = mv_total_count.
  ENDMETHOD.
  
  METHOD reset_count.
    mv_total_count = 0.
    WRITE: / 'Counter reset to 0'.
  ENDMETHOD.
  
  METHOD get_id.
    rv_id = mv_instance_id.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_obj1 TYPE REF TO lcl_counter,
        lo_obj2 TYPE REF TO lcl_counter,
        lo_obj3 TYPE REF TO lcl_counter.
  
  " Access static method without creating object
  WRITE: / 'Total count before creating objects:', 
           lcl_counter=>get_total_count( ).
  
  SKIP.
  
  " Create objects
  CREATE OBJECT: lo_obj1, lo_obj2, lo_obj3.
  
  SKIP.
  
  " Access static attribute via class name
  WRITE: / 'Total count via class name:', lcl_counter=>mv_total_count.
  
  " Access static attribute via object reference (not recommended)
  WRITE: / 'Total count via object:', lo_obj1->mv_total_count.
  
  SKIP.
  
  " Instance attributes are unique
  WRITE: / 'Object 1 ID:', lo_obj1->mv_instance_id.
  WRITE: / 'Object 2 ID:', lo_obj2->mv_instance_id.
  WRITE: / 'Object 3 ID:', lo_obj3->mv_instance_id.
```

## 8.3 Singleton Pattern Using Static Components

```abap
REPORT z_singleton_pattern.

CLASS lcl_configuration DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      get_instance RETURNING VALUE(ro_instance) TYPE REF TO lcl_configuration.
    
    METHODS:
      set_value IMPORTING iv_key   TYPE string
                          iv_value TYPE string,
      get_value IMPORTING iv_key        TYPE string
                RETURNING VALUE(rv_value) TYPE string,
      display_all.
    
  PRIVATE SECTION.
    CLASS-DATA: mo_instance TYPE REF TO lcl_configuration.
    
    TYPES: BEGIN OF ty_config,
             key   TYPE string,
             value TYPE string,
           END OF ty_config.
    
    DATA: mt_config TYPE TABLE OF ty_config.
    
ENDCLASS.

CLASS lcl_configuration IMPLEMENTATION.
  METHOD get_instance.
    " Create instance only if it doesn't exist
    IF mo_instance IS INITIAL.
      CREATE OBJECT mo_instance.
      WRITE: / 'Configuration instance created.'.
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.
  
  METHOD set_value.
    DATA: ls_config TYPE ty_config.
    
    " Check if key exists
    READ TABLE mt_config INTO ls_config WITH KEY key = iv_key.
    IF sy-subrc = 0.
      " Update existing
      MODIFY TABLE mt_config FROM VALUE #( key = iv_key value = iv_value ).
    ELSE.
      " Add new
      ls_config-key = iv_key.
      ls_config-value = iv_value.
      APPEND ls_config TO mt_config.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_value.
    DATA: ls_config TYPE ty_config.
    READ TABLE mt_config INTO ls_config WITH KEY key = iv_key.
    IF sy-subrc = 0.
      rv_value = ls_config-value.
    ENDIF.
  ENDMETHOD.
  
  METHOD display_all.
    DATA: ls_config TYPE ty_config.
    
    WRITE: / '=== Configuration Settings ==='.
    LOOP AT mt_config INTO ls_config.
      WRITE: / ls_config-key, '=', ls_config-value.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_config1 TYPE REF TO lcl_configuration,
        lo_config2 TYPE REF TO lcl_configuration.
  
  " Get instance
  lo_config1 = lcl_configuration=>get_instance( ).
  
  " Set some values
  lo_config1->set_value( iv_key = 'SERVER' iv_value = 'sap-prod-01' ).
  lo_config1->set_value( iv_key = 'PORT' iv_value = '8080' ).
  lo_config1->set_value( iv_key = 'TIMEOUT' iv_value = '30' ).
  
  SKIP.
  
  " Get another instance reference
  lo_config2 = lcl_configuration=>get_instance( ).
  
  " Both references point to the same object
  lo_config2->set_value( iv_key = 'DEBUG' iv_value = 'true' ).
  
  SKIP.
  
  " Display from first reference - shows all values including DEBUG
  lo_config1->display_all( ).
  
  SKIP.
  
  " Prove they are the same object
  IF lo_config1 = lo_config2.
    WRITE: / 'Both references point to the same instance.'.
  ENDIF.
```

---

# CHAPTER 9: INHERITANCE - CODE REUSABILITY

## 9.1 Understanding Inheritance

Inheritance allows a class to acquire properties and behaviors from another class.

```
┌─────────────────────────────────────────────────────────────┐
│                      INHERITANCE                            │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│              ┌─────────────────┐                            │
│              │    VEHICLE      │  ← Parent/Super/Base Class │
│              │ ─────────────── │                            │
│              │ - brand         │                            │
│              │ - speed         │                            │
│              │ + start()       │                            │
│              │ + stop()        │                            │
│              └────────┬────────┘                            │
│                       │                                     │
│           ┌───────────┴───────────┐                         │
│           ↓                       ↓                         │
│    ┌─────────────┐         ┌─────────────┐                  │
│    │     CAR     │         │ MOTORCYCLE  │ ← Child/Sub     │
│    │ ─────────── │         │ ─────────── │   Classes       │
│    │ - doors     │         │ - type      │                  │
│    │ + openDoor()│         │ + wheelie() │                  │
│    └─────────────┘         └─────────────┘                  │
│                                                             │
│    Child classes inherit brand, speed, start(), stop()      │
│    from parent class                                        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 9.2 Basic Inheritance Example

```abap
REPORT z_inheritance_demo.

*----------------------------------------------------------------------*
*       CLASS lcl_vehicle DEFINITION (Parent Class)
*----------------------------------------------------------------------*
CLASS lcl_vehicle DEFINITION.
  PUBLIC SECTION.
    DATA: mv_brand TYPE string,
          mv_model TYPE string,
          mv_year  TYPE i.
    
    METHODS: constructor IMPORTING iv_brand TYPE string
                                   iv_model TYPE string
                                   iv_year  TYPE i,
             start,
             stop,
             display_info.
    
  PROTECTED SECTION.
    DATA: mv_is_running TYPE abap_bool.
    
ENDCLASS.

CLASS lcl_vehicle IMPLEMENTATION.
  METHOD constructor.
    mv_brand = iv_brand.
    mv_model = iv_model.
    mv_year = iv_year.
    mv_is_running = abap_false.
  ENDMETHOD.
  
  METHOD start.
    mv_is_running = abap_true.
    WRITE: / mv_brand, mv_model, 'is starting...'.
  ENDMETHOD.
  
  METHOD stop.
    mv_is_running = abap_false.
    WRITE: / mv_brand, mv_model, 'is stopping...'.
  ENDMETHOD.
  
  METHOD display_info.
    WRITE: / 'Brand:', mv_brand,
           / 'Model:', mv_model,
           / 'Year:', mv_year.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_car DEFINITION (Child Class)
*----------------------------------------------------------------------*
CLASS lcl_car DEFINITION INHERITING FROM lcl_vehicle.
  PUBLIC SECTION.
    DATA: mv_num_doors TYPE i.
    
    METHODS: constructor IMPORTING iv_brand     TYPE string
                                   iv_model     TYPE string
                                   iv_year      TYPE i
                                   iv_num_doors TYPE i,
             open_sunroof,
             display_info REDEFINITION.  " Override parent method
    
ENDCLASS.

CLASS lcl_car IMPLEMENTATION.
  METHOD constructor.
    " Call parent constructor
    super->constructor( iv_brand = iv_brand
                        iv_model = iv_model
                        iv_year  = iv_year ).
    mv_num_doors = iv_num_doors.
  ENDMETHOD.
  
  METHOD open_sunroof.
    IF mv_is_running = abap_true.  " Can access PROTECTED attribute
      WRITE: / 'Opening sunroof of', mv_brand, mv_model.
    ELSE.
      WRITE: / 'Start the car first!'.
    ENDIF.
  ENDMETHOD.
  
  METHOD display_info.
    " Call parent implementation
    super->display_info( ).
    " Add car-specific info
    WRITE: / 'Number of Doors:', mv_num_doors.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_motorcycle DEFINITION (Child Class)
*----------------------------------------------------------------------*
CLASS lcl_motorcycle DEFINITION INHERITING FROM lcl_vehicle.
  PUBLIC SECTION.
    DATA: mv_engine_cc TYPE i.
    
    METHODS: constructor IMPORTING iv_brand     TYPE string
                                   iv_model     TYPE string
                                   iv_year      TYPE i
                                   iv_engine_cc TYPE i,
             do_wheelie,
             display_info REDEFINITION.
    
ENDCLASS.

CLASS lcl_motorcycle IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_brand = iv_brand
                        iv_model = iv_model
                        iv_year  = iv_year ).
    mv_engine_cc = iv_engine_cc.
  ENDMETHOD.
  
  METHOD do_wheelie.
    IF mv_is_running = abap_true.
      WRITE: / mv_brand, mv_model, 'is doing a wheelie!'.
    ELSE.
      WRITE: / 'Start the motorcycle first!'.
    ENDIF.
  ENDMETHOD.
  
  METHOD display_info.
    super->display_info( ).
    WRITE: / 'Engine CC:', mv_engine_cc.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_car   TYPE REF TO lcl_car,
        lo_bike  TYPE REF TO lcl_motorcycle.
  
  " Create car
  CREATE OBJECT lo_car
    EXPORTING
      iv_brand     = 'Toyota'
      iv_model     = 'Camry'
      iv_year      = 2023
      iv_num_doors = 4.
  
  WRITE: / '=== CAR ==='.
  lo_car->display_info( ).
  SKIP.
  lo_car->start( ).
  lo_car->open_sunroof( ).
  lo_car->stop( ).
  
  SKIP.
  
  " Create motorcycle
  CREATE OBJECT lo_bike
    EXPORTING
      iv_brand     = 'Harley-Davidson'
      iv_model     = 'Sportster'
      iv_year      = 2022
      iv_engine_cc = 1200.
  
  WRITE: / '=== MOTORCYCLE ==='.
  lo_bike->display_info( ).
  SKIP.
  lo_bike->do_wheelie( ).  " Won't work - not started
  lo_bike->start( ).
  lo_bike->do_wheelie( ).  " Works now
  lo_bike->stop( ).
```

## 9.3 Inheritance Rules in ABAP

| Aspect | Rule |
|--------|------|
| Inheritance Keyword | INHERITING FROM |
| Multiple Inheritance | NOT allowed (single inheritance only) |
| Access to Parent | super-> |
| Override Method | REDEFINITION |
| Prevent Inheritance | FINAL class |
| Prevent Override | FINAL method |
| Constructor | Must call super->constructor( ) |

## 9.4 Multi-Level Inheritance

```abap
REPORT z_multilevel_inheritance.

" Level 1: Base Class
CLASS lcl_animal DEFINITION.
  PUBLIC SECTION.
    DATA: mv_name TYPE string.
    METHODS: constructor IMPORTING iv_name TYPE string,
             breathe,
             move.
ENDCLASS.

CLASS lcl_animal IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  
  METHOD breathe.
    WRITE: / mv_name, 'is breathing.'.
  ENDMETHOD.
  
  METHOD move.
    WRITE: / mv_name, 'is moving.'.
  ENDMETHOD.
ENDCLASS.

" Level 2: Inherits from Animal
CLASS lcl_mammal DEFINITION INHERITING FROM lcl_animal.
  PUBLIC SECTION.
    DATA: mv_fur_color TYPE string.
    METHODS: constructor IMPORTING iv_name      TYPE string
                                   iv_fur_color TYPE string,
             feed_young.
ENDCLASS.

CLASS lcl_mammal IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = iv_name ).
    mv_fur_color = iv_fur_color.
  ENDMETHOD.
  
  METHOD feed_young.
    WRITE: / mv_name, 'is feeding its young with milk.'.
  ENDMETHOD.
ENDCLASS.

" Level 3: Inherits from Mammal
CLASS lcl_dog DEFINITION INHERITING FROM lcl_mammal.
  PUBLIC SECTION.
    DATA: mv_breed TYPE string.
    METHODS: constructor IMPORTING iv_name      TYPE string
                                   iv_fur_color TYPE string
                                   iv_breed     TYPE string,
             bark,
             move REDEFINITION.
ENDCLASS.

CLASS lcl_dog IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = iv_name iv_fur_color = iv_fur_color ).
    mv_breed = iv_breed.
  ENDMETHOD.
  
  METHOD bark.
    WRITE: / mv_name, 'says: Woof! Woof!'.
  ENDMETHOD.
  
  METHOD move.
    WRITE: / mv_name, 'is running on four legs.'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_dog TYPE REF TO lcl_dog.
  
  CREATE OBJECT lo_dog
    EXPORTING
      iv_name      = 'Buddy'
      iv_fur_color = 'Golden'
      iv_breed     = 'Labrador'.
  
  WRITE: / '=== Dog Buddy ==='.
  WRITE: / 'Name:', lo_dog->mv_name.          " From lcl_animal
  WRITE: / 'Fur Color:', lo_dog->mv_fur_color. " From lcl_mammal
  WRITE: / 'Breed:', lo_dog->mv_breed.         " From lcl_dog
  
  SKIP.
  lo_dog->breathe( ).     " From lcl_animal
  lo_dog->move( ).        " Overridden in lcl_dog
  lo_dog->feed_young( ).  " From lcl_mammal
  lo_dog->bark( ).        " From lcl_dog
```

## 9.5 FINAL Classes and Methods

```abap
" FINAL class - Cannot be inherited
CLASS lcl_immutable_config DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS: get_setting RETURNING VALUE(rv_value) TYPE string.
ENDCLASS.

CLASS lcl_immutable_config IMPLEMENTATION.
  METHOD get_setting.
    rv_value = 'Fixed Setting'.
  ENDMETHOD.
ENDCLASS.

" This would cause syntax error:
" CLASS lcl_extended DEFINITION INHERITING FROM lcl_immutable_config.
"   " Error: Cannot inherit from FINAL class
" ENDCLASS.

" FINAL method - Cannot be overridden
CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
    METHODS: calculate FINAL RETURNING VALUE(rv_result) TYPE i.
ENDCLASS.

CLASS lcl_base IMPLEMENTATION.
  METHOD calculate.
    rv_result = 100.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_derived DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    " This would cause syntax error:
    " METHODS: calculate REDEFINITION.  " Error: Cannot override FINAL method
ENDCLASS.
```

---

# CHAPTER 10: METHOD OVERRIDING - CUSTOMIZING BEHAVIOR

## 10.1 Understanding Method Overriding

Method overriding allows a child class to provide a specific implementation for a method that is already defined in the parent class.

```
┌─────────────────────────────────────────────────────────────┐
│                   METHOD OVERRIDING                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Parent Class: lcl_shape                                   │
│   ┌─────────────────────────────────────┐                   │
│   │ + calculate_area( ) → returns 0     │                   │
│   └─────────────────────────────────────┘                   │
│                       │                                     │
│         ┌─────────────┼─────────────┐                       │
│         ↓             ↓             ↓                       │
│   ┌──────────┐  ┌──────────┐  ┌──────────┐                  │
│   │ Circle   │  │ Rectangle│  │ Triangle │                  │
│   ├──────────┤  ├──────────┤  ├──────────┤                  │
│   │calculate_│  │calculate_│  │calculate_│                  │
│   │area()    │  │area()    │  │area()    │                  │
│   │ π * r²   │  │ l * w    │  │ ½*b*h    │                  │
│   └──────────┘  └──────────┘  └──────────┘                  │
│                                                             │
│   Each child class OVERRIDES with its own formula           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 10.2 Complete Override Example

```abap
REPORT z_override_demo.

*----------------------------------------------------------------------*
*       CLASS lcl_shape DEFINITION (Parent)
*----------------------------------------------------------------------*
CLASS lcl_shape DEFINITION.
  PUBLIC SECTION.
    DATA: mv_name TYPE string.
    
    METHODS: constructor IMPORTING iv_name TYPE string,
             calculate_area RETURNING VALUE(rv_area) TYPE f,
             calculate_perimeter RETURNING VALUE(rv_perimeter) TYPE f,
             display.
    
  PROTECTED SECTION.
    CONSTANTS: c_pi TYPE f VALUE '3.14159265359'.
    
ENDCLASS.

CLASS lcl_shape IMPLEMENTATION.
  METHOD constructor.
    mv_name = iv_name.
  ENDMETHOD.
  
  METHOD calculate_area.
    " Default implementation - returns 0
    rv_area = 0.
  ENDMETHOD.
  
  METHOD calculate_perimeter.
    rv_perimeter = 0.
  ENDMETHOD.
  
  METHOD display.
    WRITE: / 'Shape:', mv_name,
           / 'Area:', me->calculate_area( ),
           / 'Perimeter:', me->calculate_perimeter( ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_circle DEFINITION (Child)
*----------------------------------------------------------------------*
CLASS lcl_circle DEFINITION INHERITING FROM lcl_shape.
  PUBLIC SECTION.
    DATA: mv_radius TYPE f.
    
    METHODS: constructor IMPORTING iv_radius TYPE f,
             calculate_area REDEFINITION,
             calculate_perimeter REDEFINITION.
    
ENDCLASS.

CLASS lcl_circle IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = 'Circle' ).
    mv_radius = iv_radius.
  ENDMETHOD.
  
  METHOD calculate_area.
    " Area = π * r²
    rv_area = c_pi * mv_radius * mv_radius.
  ENDMETHOD.
  
  METHOD calculate_perimeter.
    " Perimeter = 2 * π * r
    rv_perimeter = 2 * c_pi * mv_radius.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_rectangle DEFINITION (Child)
*----------------------------------------------------------------------*
CLASS lcl_rectangle DEFINITION INHERITING FROM lcl_shape.
  PUBLIC SECTION.
    DATA: mv_length TYPE f,
          mv_width  TYPE f.
    
    METHODS: constructor IMPORTING iv_length TYPE f
                                   iv_width  TYPE f,
             calculate_area REDEFINITION,
             calculate_perimeter REDEFINITION.
    
ENDCLASS.

CLASS lcl_rectangle IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = 'Rectangle' ).
    mv_length = iv_length.
    mv_width = iv_width.
  ENDMETHOD.
  
  METHOD calculate_area.
    " Area = length * width
    rv_area = mv_length * mv_width.
  ENDMETHOD.
  
  METHOD calculate_perimeter.
    " Perimeter = 2 * (length + width)
    rv_perimeter = 2 * ( mv_length + mv_width ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_triangle DEFINITION (Child)
*----------------------------------------------------------------------*
CLASS lcl_triangle DEFINITION INHERITING FROM lcl_shape.
  PUBLIC SECTION.
    DATA: mv_base   TYPE f,
          mv_height TYPE f,
          mv_side1  TYPE f,
          mv_side2  TYPE f,
          mv_side3  TYPE f.
    
    METHODS: constructor IMPORTING iv_base   TYPE f
                                   iv_height TYPE f
                                   iv_side1  TYPE f
                                   iv_side2  TYPE f
                                   iv_side3  TYPE f,
             calculate_area REDEFINITION,
             calculate_perimeter REDEFINITION.
    
ENDCLASS.

CLASS lcl_triangle IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_name = 'Triangle' ).
    mv_base = iv_base.
    mv_height = iv_height.
    mv_side1 = iv_side1.
    mv_side2 = iv_side2.
    mv_side3 = iv_side3.
  ENDMETHOD.
  
  METHOD calculate_area.
    " Area = (1/2) * base * height
    rv_area = '0.5' * mv_base * mv_height.
  ENDMETHOD.
  
  METHOD calculate_perimeter.
    " Perimeter = sum of all sides
    rv_perimeter = mv_side1 + mv_side2 + mv_side3.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lt_shapes TYPE TABLE OF REF TO lcl_shape,
        lo_shape  TYPE REF TO lcl_shape.
  
  DATA: lo_circle    TYPE REF TO lcl_circle,
        lo_rectangle TYPE REF TO lcl_rectangle,
        lo_triangle  TYPE REF TO lcl_triangle.
  
  " Create shapes
  CREATE OBJECT lo_circle EXPORTING iv_radius = 5.
  CREATE OBJECT lo_rectangle EXPORTING iv_length = 10 iv_width = 5.
  CREATE OBJECT lo_triangle
    EXPORTING
      iv_base   = 6
      iv_height = 4
      iv_side1  = 5
      iv_side2  = 5
      iv_side3  = 6.
  
  " Add to collection (polymorphism)
  APPEND lo_circle TO lt_shapes.
  APPEND lo_rectangle TO lt_shapes.
  APPEND lo_triangle TO lt_shapes.
  
  " Display all shapes
  LOOP AT lt_shapes INTO lo_shape.
    WRITE: / '================================'.
    lo_shape->display( ).
  ENDLOOP.
```

**Output:**
```
================================
Shape: Circle
Area: 78.5398163397
Perimeter: 31.4159265359
================================
Shape: Rectangle
Area: 50.0
Perimeter: 30.0
================================
Shape: Triangle
Area: 12.0
Perimeter: 16.0
```

## 10.3 Using SUPER to Call Parent Implementation

```abap
REPORT z_super_demo.

CLASS lcl_document DEFINITION.
  PUBLIC SECTION.
    DATA: mv_doc_id TYPE char10,
          mv_title  TYPE string.
    
    METHODS: constructor IMPORTING iv_id    TYPE char10
                                   iv_title TYPE string,
             print.
    
ENDCLASS.

CLASS lcl_document IMPLEMENTATION.
  METHOD constructor.
    mv_doc_id = iv_id.
    mv_title = iv_title.
  ENDMETHOD.
  
  METHOD print.
    WRITE: / '=============================',
           / 'Document ID:', mv_doc_id,
           / 'Title:', mv_title,
           / '============================='.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_invoice DEFINITION INHERITING FROM lcl_document.
  PUBLIC SECTION.
    DATA: mv_amount    TYPE p LENGTH 10 DECIMALS 2,
          mv_customer  TYPE string.
    
    METHODS: constructor IMPORTING iv_id       TYPE char10
                                   iv_title    TYPE string
                                   iv_amount   TYPE p
                                   iv_customer TYPE string,
             print REDEFINITION.
    
ENDCLASS.

CLASS lcl_invoice IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id = iv_id iv_title = iv_title ).
    mv_amount = iv_amount.
    mv_customer = iv_customer.
  ENDMETHOD.
  
  METHOD print.
    " Call parent implementation first
    super->print( ).
    
    " Add invoice-specific information
    WRITE: / 'Customer:', mv_customer,
           / 'Amount:', mv_amount,
           / '============================='.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_invoice TYPE REF TO lcl_invoice.
  
  CREATE OBJECT lo_invoice
    EXPORTING
      iv_id       = 'INV001'
      iv_title    = 'Monthly Services Invoice'
      iv_amount   = '5000.00'
      iv_customer = 'ABC Corporation'.
  
  lo_invoice->print( ).
```

## 10.4 Exercise: Employee Hierarchy

**Problem:** Create an employee management system with the following hierarchy:

```
                lcl_employee (Base)
                      │
        ┌─────────────┼─────────────┐
        │             │             │
   lcl_manager   lcl_developer   lcl_intern
```

**Requirements:**
1. Base class has: emp_id, name, base_salary, calculate_salary()
2. Manager: has team_size, gets 10% bonus per team member
3. Developer: has programming_language, gets 15% skill bonus
4. Intern: has university, gets 50% of base salary

### Solution:

```abap
REPORT z_employee_hierarchy.

*----------------------------------------------------------------------*
*       CLASS lcl_employee DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_employee DEFINITION.
  PUBLIC SECTION.
    DATA: mv_emp_id      TYPE numc6 READ-ONLY,
          mv_name        TYPE string READ-ONLY,
          mv_base_salary TYPE p LENGTH 10 DECIMALS 2 READ-ONLY.
    
    METHODS: constructor IMPORTING iv_id     TYPE numc6
                                   iv_name   TYPE string
                                   iv_salary TYPE p,
             calculate_salary RETURNING VALUE(rv_salary) TYPE p,
             get_designation RETURNING VALUE(rv_designation) TYPE string,
             display_info.
    
  PROTECTED SECTION.
    DATA: mv_designation TYPE string VALUE 'Employee'.
    
ENDCLASS.

CLASS lcl_employee IMPLEMENTATION.
  METHOD constructor.
    mv_emp_id = iv_id.
    mv_name = iv_name.
    mv_base_salary = iv_salary.
  ENDMETHOD.
  
  METHOD calculate_salary.
    rv_salary = mv_base_salary.
  ENDMETHOD.
  
  METHOD get_designation.
    rv_designation = mv_designation.
  ENDMETHOD.
  
  METHOD display_info.
    WRITE: / '┌────────────────────────────────────┐',
           / '│ Employee ID   :', mv_emp_id,
           / '│ Name          :', mv_name,
           / '│ Designation   :', me->get_designation( ),
           / '│ Base Salary   :', mv_base_salary,
           / '│ Final Salary  :', me->calculate_salary( ),
           / '└────────────────────────────────────┘'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_manager DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_manager DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    DATA: mv_team_size TYPE i.
    
    METHODS: constructor IMPORTING iv_id        TYPE numc6
                                   iv_name      TYPE string
                                   iv_salary    TYPE p
                                   iv_team_size TYPE i,
             calculate_salary REDEFINITION,
             display_info REDEFINITION.
    
ENDCLASS.

CLASS lcl_manager IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id = iv_id iv_name = iv_name iv_salary = iv_salary ).
    mv_team_size = iv_team_size.
    mv_designation = 'Manager'.
  ENDMETHOD.
  
  METHOD calculate_salary.
    " Base salary + 10% bonus per team member
    DATA: lv_bonus TYPE p LENGTH 10 DECIMALS 2.
    lv_bonus = mv_base_salary * ( mv_team_size * 10 ) / 100.
    rv_salary = mv_base_salary + lv_bonus.
  ENDMETHOD.
  
  METHOD display_info.
    super->display_info( ).
    WRITE: / '│ Team Size     :', mv_team_size,
           / '│ Team Bonus    :', mv_team_size * 10, '%',
           / '└────────────────────────────────────┘'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_developer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_developer DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    DATA: mv_prog_language TYPE string.
    
    METHODS: constructor IMPORTING iv_id       TYPE numc6
                                   iv_name     TYPE string
                                   iv_salary   TYPE p
                                   iv_language TYPE string,
             calculate_salary REDEFINITION,
             display_info REDEFINITION.
    
ENDCLASS.

CLASS lcl_developer IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id = iv_id iv_name = iv_name iv_salary = iv_salary ).
    mv_prog_language = iv_language.
    mv_designation = 'Developer'.
  ENDMETHOD.
  
  METHOD calculate_salary.
    " Base salary + 15% skill bonus
    DATA: lv_bonus TYPE p LENGTH 10 DECIMALS 2.
    lv_bonus = mv_base_salary * 15 / 100.
    rv_salary = mv_base_salary + lv_bonus.
  ENDMETHOD.
  
  METHOD display_info.
    super->display_info( ).
    WRITE: / '│ Language      :', mv_prog_language,
           / '│ Skill Bonus   : 15%',
           / '└────────────────────────────────────┘'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_intern DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_intern DEFINITION INHERITING FROM lcl_employee.
  PUBLIC SECTION.
    DATA: mv_university TYPE string.
    
    METHODS: constructor IMPORTING iv_id         TYPE numc6
                                   iv_name       TYPE string
                                   iv_salary     TYPE p
                                   iv_university TYPE string,
             calculate_salary REDEFINITION,
             display_info REDEFINITION.
    
ENDCLASS.

CLASS lcl_intern IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_id = iv_id iv_name = iv_name iv_salary = iv_salary ).
    mv_university = iv_university.
    mv_designation = 'Intern'.
  ENDMETHOD.
  
  METHOD calculate_salary.
    " 50% of base salary
    rv_salary = mv_base_salary * 50 / 100.
  ENDMETHOD.
  
  METHOD display_info.
    super->display_info( ).
    WRITE: / '│ University    :', mv_university,
           / '│ Intern Rate   : 50%',
           / '└────────────────────────────────────┘'.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lt_employees TYPE TABLE OF REF TO lcl_employee,
        lo_emp       TYPE REF TO lcl_employee.
  
  DATA: lo_manager   TYPE REF TO lcl_manager,
        lo_developer TYPE REF TO lcl_developer,
        lo_intern    TYPE REF TO lcl_intern.
  
  " Create employees
  CREATE OBJECT lo_manager
    EXPORTING
      iv_id        = '000001'
      iv_name      = 'John Smith'
      iv_salary    = '80000.00'
      iv_team_size = 5.
  
  CREATE OBJECT lo_developer
    EXPORTING
      iv_id       = '000002'
      iv_name     = 'Jane Doe'
      iv_salary   = '65000.00'
      iv_language = 'ABAP'.
  
  CREATE OBJECT lo_intern
    EXPORTING
      iv_id         = '000003'
      iv_name       = 'Bob Wilson'
      iv_salary     = '30000.00'
      iv_university = 'MIT'.
  
  " Add to collection
  APPEND lo_manager TO lt_employees.
  APPEND lo_developer TO lt_employees.
  APPEND lo_intern TO lt_employees.
  
  " Display all employees
  WRITE: / '╔════════════════════════════════════════╗'.
  WRITE: / '║        EMPLOYEE SALARY REPORT          ║'.
  WRITE: / '╚════════════════════════════════════════╝'.
  
  DATA: lv_total_salary TYPE p LENGTH 12 DECIMALS 2.
  
  LOOP AT lt_employees INTO lo_emp.
    SKIP.
    lo_emp->display_info( ).
    lv_total_salary = lv_total_salary + lo_emp->calculate_salary( ).
  ENDLOOP.
  
  SKIP.
  WRITE: / '════════════════════════════════════════'.
  WRITE: / 'TOTAL SALARY EXPENSE:', lv_total_salary.
  WRITE: / '════════════════════════════════════════'.
```

---

# COMMON ISSUES AND TROUBLESHOOTING - PART 1

## Issue 1: Object Not Instantiated

```abap
" WRONG - Will cause runtime error
DATA: lo_obj TYPE REF TO lcl_class.
lo_obj->method( ).  " ERROR: Object reference is initial

" CORRECT
DATA: lo_obj TYPE REF TO lcl_class.
CREATE OBJECT lo_obj.  " Create object first
lo_obj->method( ).     " Now it works
```

## Issue 2: Calling Parent Constructor

```abap
" WRONG - Constructor not called
CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_value TYPE i.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD constructor.
    mv_value = iv_value.  " Parent attributes may not be initialized
  ENDMETHOD.
ENDCLASS.

" CORRECT - Always call parent constructor
CLASS lcl_child IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_value = iv_value ).  " Call parent first
    " Then child-specific logic
  ENDMETHOD.
ENDCLASS.
```

## Issue 3: Accessing Private Members in Child Class

```abap
" WRONG
CLASS lcl_parent DEFINITION.
  PRIVATE SECTION.
    DATA: mv_secret TYPE i.
ENDCLASS.

CLASS lcl_child DEFINITION INHERITING FROM lcl_parent.
  PUBLIC SECTION.
    METHODS: expose_secret.
ENDCLASS.

CLASS lcl_child IMPLEMENTATION.
  METHOD expose_secret.
    WRITE: mv_secret.  " ERROR: Private members not accessible
  ENDMETHOD.
ENDCLASS.

" CORRECT - Use PROTECTED instead
CLASS lcl_parent DEFINITION.
  PROTECTED SECTION.  " Changed from PRIVATE
    DATA: mv_secret TYPE i.
ENDCLASS.
```

---

