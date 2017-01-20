# rpg-intro
Introduction to RPG

## Todo

* Prototypes
* Modules and programs
* Creating commands for programs
* Embedded SQL basics

## Contents

1. IBM i file system and the IFS
2. IBM i commands
3. Creating your first RPG program.
4. RPG Syntax
5. RPG data-types
6. RPG Procedures
7. RPG subroutines
8. RPG data-structures

## IBM i file system and the IFS 

The two file systems we’re going to look at are 

* The 'library file system'
* The IFS (Integrated-File-System) 

The reason we’re going to look at these is because you can develop RPG in either, but their differences must be explained. Like most other operating system, you can create a simple stream file and write your code inside of this file and the compiler will do its work – this is a still a normal concept on IBM i, but most industries do not do this. 

You’ll find that most IBM i shops will develop in something called ‘source members’. A ‘source member’ is a DB2 concept. A table within DB2 can contain multiple members which share the same columns, but there are special tables called ‘source physical files’ (SPF for short). ‘Physical file’ (PF for short) is a legacy name for tables, but the name will continue to live on while people develop in source members. A physical file is a type of object and all objects live within a library.  

Our future programs, modules, commands, and tables will all live within a library and each object name is a maximum of 10 characters long. Libraries cannot contain other libraries (except for QSYS, which is where every library lives). A library is just another type of object. 
Each source physical file has a record length. When you create a source physical file (CRTSRCPF) you can specify a record length. A normal length for an SPF is 112. Whatever length you supply to the command, minus 12 – this result is the max length of each line when editing source code. 

The reason we minus 12 is because the first 12 bytes are used for the line sequence number and source date (when that line was last updated). Now note, this is only for source physical files – you don’t have to worry about line length with stream files.

## IBM i commands 
Commands on IBM i are the simplest form of commands ever invented. Commands are a maximum of 10 characters long (because they’re an object) and are made up of word abbreviations. For example:

![](https://raw.githubusercontent.com/WorksOfBarry/rpg-intro/master/assets/table1.PNG)

Concatenating the abbreviations can make up commands, which are also pronounceable:

![](https://raw.githubusercontent.com/WorksOfBarry/rpg-intro/master/assets/table2.PNG)

Some of the commands we’ll be using throughout the lectures are the following:

![](https://raw.githubusercontent.com/WorksOfBarry/rpg-intro/master/assets/table3.PNG)

When using commands on the IBM i, if you are unsure of the parameters you are able to prompt the command. You can prompt after you have entered the command and then pressed F4. This will bring up a list of parameters available for a command. You may also have the option to use F10, which will show more available parameters. 

If you’re searching for a command, you can type the start of the command followed by an asterisk. For example:

* `CRT*` will show you all commands starting with `CRT`
* `WRK*` will show you all commands starting with `WRK`

## Creating your first RPG program.

First, we will need to do the following steps:

1. Create a library (`CRTLIB`)
2. Create a source physical file (`CRTSRCPF`)
3. Create a source member (`ADDPFM`)

You may also do this within Rational Developer for i, but it’s very useful to memorize these commands for when you don’t have the IDE.

It is possible to write all your RPG code in stream files on the IFS. The easiest way to create a stream file is with Rational Developer for i.

```
**FREE  

//Declare lText standalone field/variable 
Dcl-S lText Char(25);  

//Assign a value to field 
lText = 'Hello world.';  

//Display the lText value. 
Dsply lText;  

//Set Indicator LR to *On (true) and return 
*InLR = *On; 
Return;  
```

Once we have written our code, we are going to use CRTBNDRPG to create our program object. What makes CRTBNDPGM useful is that it just creates a program object – it’s automating the CRTRPGMOD step. Another way we could have created our program object is by using CRTRPGMOD and then CRTPGM over that module. 

CRTBNDRPG does allow you to compile both stream files and source members.

* To compile a source member: `CRTBNDRPG PGM(MYLIB/XMPLE1) SRCFILE(MYLIB/QRPGLESRC) SRCMBR(XMPLE1) TEXT('My RPG IV Program')`
* To compile a source member: `CRTBNDRPG PGM(MYLIB/XMPLE1) SRCSTMF('xmple.rpgle') TEXT('My RPG IV Program')`

Note that when you compile a stream file, the SRCSTMF path should be relative to your current directory when running the compild.

You are now going to call your program using `CALL <programname>`, which should then display ‘Hello world’ on your terminal. 

## RPG Syntax

There are lots of variations of RPG syntax. For all the RPG we write, we will be using free format. 

Notice how in the last lecture, we used `**FREE` – this is a compiler directive. It tells the compiler, we’re going to write our free format code from the start of the line. If we didn’t have that directive, we’d have to start each line at the 8th index (7 spaces, then start the code). If used, `**FREE` must be on the first line and the first 6 characters with nothing following. 

`*InLR = *On` tells the runtime that we’re on the last record. We need this because some elements of legacy RPG are still supported. Without it, the program would keep looping until LR was on. 

In RPG, things like IF, DOW, DOU, SELECT, WHEN, DCL-S, etc are all RPG operations. You will always need a space between the operation code and the next value. For good practice, you should surround expressions with brackets.

```
//Good practice
If (2 > 1);
  Dsply 'True';
ENDIF;

//Bad practice
If 2 > 1;
  Dsply 'True';
ENDIF; 
```

Variables are case-insensitive and cannot start with numeric digits (but can contain them). Variables must be defined at the top of your program/procedure It is good practice to start variables in the global scope with a lowercase 'g' – variables in a local scope to start with a lowercase 'l'.

```
**FREE
Ctl-Opt DFTACTGRP(*No);

Dcl-S gMyVar Char(10);

MyProc();

*InLR = *On;
Return;

Dcl-Proc MyProc;
  Dcl-S lMyVar Char(10);

  gMyVar = 'Hello';
  lMyVar = 'World';
END-PROC;    
```

## RPG data-types

RPG consists of lots of data-types, in this lecture we’re going to cover a few of them. You declare variables with the ‘Dcl-S’ operation, followed by the name, type and then optional keywords.

```
Dcl-S name type [keywords]
```

Here are some of the types we’re going to use in our lectures:

![](https://raw.githubusercontent.com/WorksOfBarry/rpg-intro/master/assets/table4.PNG)

When you use character fields in expressions, it will always trim the right blanks from the variable. For example:

```
Dcl-S lMyVarA Char(20)  Inz('Hello!');
Dcl-S lMyVarB Char(100) Inz('Hello!'); 

If (lMyVarA = lMyVarB); 
  Dsply 'True';
ENDIF;
```

Is actually computed as:

```
If (%TrimR(lMyVarA) = %TrimR(lMyVarB));
  Dsply 'True';
ENDIF;  
```

RPG provides lots of keywords when declaring both variables and data-structures (more on data-structures in a future Lecture). One of those is `DIM` (dimensions), which can be used on both `Dcl-S` and `Dcl-Ds`. The `DIM` keyword allows you declare arrays, where the length is specified in the ‘DIM’ keyword. You can reference array elements the same way you would call a procedure.

```
Ctl-Opt DftActGrp(*No);

Dcl-S MyArray Char(10) Dim(10);

MyArray(1) = 'Value1';
MyArray(5) = 'Value5';
```

Arrays indexes in RPG always start at 1. Whenever you’re referencing the length of an array, never use a hardcoded constant, instead use the ‘%Elem’ built-in function (elements). In this example, we will use this built-in function to clear every element in the array.

```
Dcl-S index   Int(3);
Dcl-S MyArray Char(10) Dim(10);

MyArray(1) = 'Value1';
MyArray(5) = 'Value5';

For index = 1 to %Elem(MyArray);
  MyArray(index) = *Blank;
ENDFOR;
```

While this code is valid and working, there is an easier option to clearing fields, arrays and data-structures. Instead of using this `FOR` loop to clear the, we can use the `CLEAR` operation code. The `CLEAR` operation code works on all fields, arrays, and data-structures (and subfields) – for every type too:

```
Dcl-S index   Int(3);
Dcl-S MyArray Char(10) Dim(10);

MyArray(1) = 'Value1';
MyArray(5) = 'Value5';

Clear MyArray;
```

Another useful keyword is `INZ` (initialization, US spelling). This allows the developer to give variables an initial value when they are declared. You would have already seen this used in this Lecture and will in future lectures too. If you use ‘INZ’ with ‘DIM’, it will give every element that initial value. There is a handy extra operation code, called `RESET`. `RESET` allows you to reset fields, arrays, and data-structure subfields back to their initial value (defined with the ‘INZ’ keyword):

```
Dcl-S index   Int(3);
Dcl-S MyArray Char(10) Dim(10) Inz('Value');

MyArray(1) = 'Nothing!';
MyArray(5) = 'Nothing!';

Reset MyArray;
```

## RPG Procedures

Procedures in RPG are very comparable to functions in C. If you studied Lecture 5, you would have already seen an example of a basic procedure in RPG.

Procedures can:

* Have a return type (or void)
* Have a parameter list
* Have parameter by refernece, constant or value

The syntax of a procedure can be confusing at first, but after practice it will become much simpler.

```
dcl-proc name [export];
  [dcl-pr *N [returntype] [end-pi];]
    [parmname parmtype passby;]
  [end-pi;]
end-proc;
```

`Dcl-Proc` stands for `declare procedure` and `Dcl-Pi` stands for `declare procedure-interface`. A PI is required for any procedure that has parameters or a return value. It’s used so the compiler knows how to invoke it correctly.

### Procedure with no parameters or return value

```
**FREE
Ctl-Opt DFTACTGRP(*No);

Dcl-S gMyVar Char(20) Inz('Hello!');

Dsply gMyVar;
MyProc();
Dsply gMyVar;

*InLR = *On;
Return;

Dcl-Proc MyProc;
  gMyVar = 'Goodbye!';
END-PROC;
```

In this example, after MyProc() has been called – it will update the global variable to a different value. Notice how there is no PI because there are no parameters or return value.

### Procedure with no parameters and return value

```
**FREE
Ctl-Opt DFTACTGRP(*No);

Dcl-S gMyVar Char(20) Inz('Hello!');

Dsply gMyVar;
gMyVar = MyProc();
Dsply gMyVar;

*InLR = *On;
Return;

Dcl-Proc MyProc;
  Dcl-Pi *N Char(8) End-Pi;

  Return 'Goodbye!';
END-PROC;
```

In this example, calling MyProc() returns a Char(8) value and assigns it to gMyVar. Notice how the `Dcl-Pi` and `End-Pi` are on the same line because there are no parameters for this procedure. `*N` represents nothing/blank. You are able to replace it with the procedure name, but it is not required.

### Procedure with a parameter and return value.

```
**FREE
Ctl-Opt DFTACTGRP(*No);

Dcl-S gMyVar Char(20) Inz('Hello!');

Dsply gMyVar;
gMyVar = MyProc('Goodbye');
Dsply gMyVar;

*InLR = *On;
Return;

Dcl-Proc MyProc;
  Dcl-Pi *N Char(8);
    pValue Char(20) Const;
  END-PI;

  Return %Trim(pValue) + '!';
END-PROC;
```

In this example, we call MyProc() with a parameter and returns my parameter with an exclamation mark concatenated on the end. We use the ‘const’ option here so we can pass in a constant value (or expression). Notice that ‘End-Pi’ is after the parameter definition.

## RPG subroutines

A subroutine is a block of code that can be executed in the current scope and has access to all variables in the current and global scope. Variables cannot be defined in a subroutine and subroutines must be defined after the return point of your program/procedure. Mainlines can only access subroutines within the global scope and procedures can only access subroutines in the local scope (of that procedure).

The following operations are used for subroutines:

![](https://raw.githubusercontent.com/WorksOfBarry/rpg-intro/master/assets/table5.PNG)

```
**FREE

Ctl-Opt DftActGrp(*No);

Dcl-S gMyGlobal Int(3);

gMyGlobal = 0;
Dsply ('Program start.');
Exsr GlobalSub;
MyProc();
Dsply ('Program end: ' + %Char(gMyGlobal));

*InLR = *On;
Return;

Begsr GlobalSub;
  Dsply ('Entered subroutine.');
  gMyGlobal += 1;
  Dsply ('End of subroutine.');
ENDSR;

Dcl-Proc MyProc;
  Dcl-S lMyLocal Int(3);

  lMyLocal = 0;
  Dsply ('Entered my procedure.');
  Exsr LocalSub;
  Dsply ('End of my procedure.');

  Return;

  Begsr LocalSub;
    Dsply ('Entered local subroutine.');
    lMyLocal  += 1;
    gMyGlobal += 1;
    Dsply ('End of local subroutine.');
  ENDSR;
END-PROC;                      
```

## RPG data-structures

Simply put, data-structures (a DS) in RPG are a set of fields that are grouped together (even in memory). Data-structures contain subfields which can be of any type and they can also contain sub-data-structures (A DS, within a DS). When declaring subfields, you can use any of the keywords you’d use when declaring a regular field (e.g `INZ`, `DIM`).

```
dcl-ds name [keywords];
  subfield type [keywords];
  …
end-ds;
```

```
//Non-qualified DS
Dcl-Ds MyDS;
  SubfieldA Char(10);
  SubfieldB Int(10);
  SubfieldC Packed(11:2);
END-DS;

SubfieldA = 'Subfield';
SubfieldB = 1337;
SubfieldC = 363424.75;
```

It is important to know what a qualified DS and non-qualified DS is. Qualified simply means that when you reference it, you must append the data-structure name to the left. For example, let’s make the previous DS example qualified.

```
Dcl-Ds MyDS Qualified;
  SubfieldA Char(10);
  SubfieldB Int(10);
  SubfieldC Packed(11:2);
END-DS;

MyDS.SubfieldA = 'Subfield';
MyDS.SubfieldB = 1337;
MyDS.SubfieldC = 363424.75;
```

Data-structure templating is a useful thing to know if you like to write clean code. A DS template is a data-structure that is not defined at runtime, but is known to the compiler so you can copy it’s subfields into another data-structure.

```
Dcl-Ds MyDS_Temp Template Qualified;
  SubfieldA Char(10);
  SubfieldB Int(10);
  SubfieldC Packed(11:2);
END-DS;

Dcl-Ds MyRealDS  LikeDS(MyDS_Temp);
Dcl-Ds MyOtherDS LikeDS(MyDS_Temp);

MyRealDS.SubfieldB  = 1337;
MyOtherDS.SubfieldB = 1337;
```

Note that you do not have to use `LIKEDS` on a data-structure with the `TEMPLATE` keyword – you can use it on any data-structure.

## RPG Prototypes

In RPG, each program and procedure can have an interface. We learned about procedure interfaces in a previous chapter, but we didn't learn that programs can also have a PI (procedure/program interface). 

You can [http://www.ibm.com/support/knowledgecenter/ssw_ibm_i_72/rzasd/freeinterface.htm](read more here) for more info.

In general, program interfaces can: 

* Have pass by reference and const parameters.
* The value after `Dcl-PI` should be the program name.


```
**FREE

Dcl-Pi XMPLE1;
  pName Char(10)
End-Pi;

Dsply pNams;

*InLR = *On;
Return;
```