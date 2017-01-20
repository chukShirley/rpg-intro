# rpg-intro
Introduction to RPG

## Contents

1. IBM i file system and the IFS
2. IBM i commands


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

//Table 1

Concatenating the abbreviations can make up commands, which are also pronounceable:

//Table 2

Some of the commands we’ll be using throughout the lectures are the following:

//Table 3

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

You are now going to call your program using `CALL <programname>`, which should then display ‘Hello world’ on your terminal. 

## RPG Syntax

There are lots of variations of RPG syntax. For all the RPG we write, we will be using free format. 

Notice how in the last lecture, we used `**FREE` – this is a compiler directive. It tells the compiler, we’re going to write our free format code from the start of the line. If we didn’t have that directive, we’d have to start each line at the 8th index (7 spaces, then start the code). If used, `**FREE` must be on the first line and the first 6 characters with nothing following. 

`*InLR = *On` tells the runtime that we’re on the last record. We need this because some elements of legacy RPG are still supported. Without it, the program would keep looping until LR was on. 

In RPG, things like IF, DOW, DOU, SELECT, WHEN, DCL-S, etc are all RPG operations. You will always need a space between the operation code and the next value. For good practice, you should surround expressions with brackets.

