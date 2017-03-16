# php2rpg
Guide showing you PHP and RPG differences

* RPG is strongly typed. 
* RPG indexes start at 1, where PHP starts at 0.
* Variables in PHP start with a Dollar sign. ($)

***

### Typing

* RPG does not have strings like PHP. RPG has character fields, which consists of a block of memory. When you refernece the field, it will return every character that was allocated for it - including blanks.
* There are more types than shown here.

```php
$x = "Hello world!"; //String
$y = 5985;           //Int
$z = 10.365;         //Float
$a = true;           //Boolean
```

```
Dcl-S x Char(12) Inz('Hello world!');
Dcl-S y Int(5)   Inz(5985);
Dcl-S z Float(8) Inz(10.365);
Dcl-S a Ind      Inz(*On);
```

**Options for integer types**:

* `Dcl-S y Int(3|5|10|20)` (signed)
  * `3`: -128 to 127
  * `5`: -32768 to 32767
  * `10`: -2147483648 to 2147483647
  * `20`: -9223372036854775808 to 9223372036854775807
* `Dcl-S y Unz(3|5|10|20)` (unsigned)
  * `3`: 0 to 255
  * `5`: 0 to 65535
  * `10`: 0 to 4294967295
  * `20`: 0 to 18446744073709551615

**Options for decimal types**:

* `Dcl-S z Float(4|8)`
* `Dcl-S z Zoned(length:digits)` (zoned decimal)`
* `Dcl-S z Packed(length:digits)` (packed decimal)`
* `Dcl-S z Bindec(length:digits)` (binary decimal)`

***

### Create different variables
```php
$txt = "Hello world!";
$x = 5;
$y = 10.5;

echo $txt;
echo "<br>";
echo $x;
echo "<br>";
echo $y;
```

```
Dcl-S txt Char(12)    Inz('Hello world!');
Dcl-S x   Int(3)      Inz(5);
Dcl-S y   Packed(5:1) Inz(10.5);

DSPLY txt;
DSPLY %Char(x);
DSPLY %Char(y);
```

***

### Arithmetic operators
```php
echo 5 + 5;
echo 5 - 5;
echo 5 / 5;
echo 5 * 5;
```

```
Dcl-S Result Int(5); 

Result = 5 + 5;
dsply %char(Result);

Result = 5 - 5;
dsply %char(Result);

Result = 5 / 5;
dsply %char(Result);

Result = 5 * 5;
dsply %char(Result);
```

***

### Assignment operators
```php
$x = 10; 
$x += 10;
$x -= 10;
$x /= 10;
$x *= 10; 
```

```
Dcl-S Result Int(5); 

Result  = 10;
Result += 10;
Result -= 10;
Result /= 10;
Result *= 10;
```

***

### Comparison operators

* Booleans in PHP are either true of false
* Booleans are called Indicators in RPG
  * They are either *On or *Off (the same are true or false)

```php
$varA = 5;
%varB = 10;

$bool = ($varA == 4varB);
$bool = ($varA != $varB);
$bool = ($varA > $varB);
$bool = ($varA < $varB);
$bool = ($varA >= $varB);
$bool = ($varA <= $varB);
```

```

Dcl-S bool Ind;
Dcl-S varA Int(3) Inz(5);
Dcl-S varb Int(3) Inz(10);

bool = (varA == varB);
bool = (varA <> varB);
bool = (varA > varB);
bool = (varA < varB);
bool = (varA >= varB);
bool = (varA <= varB);
```

***

### Logical operators
```php
If ($bool == true && $varA == 5) {
  echo('Hello world.');
]

If ($bool == false || $varA == 5) {
  echo('Hello world.');
]

If ($bool != false) {
  echo('Hello world.');
]
```

```
If (bool = *On AND varA = 5);
  dsply ('Hello world.');
Endif;

If (bool = *Off OR varA = 5);
  dsply ('Hello world.');
Endif;

If NOT (bool = *Off);
  dsply ('Hello world.');
Endif;
```

***

### Arrays

```php
$MyArray = [];
$MyArray[0] = 'Hello';
$MyArray[1] = 'World';
$MyArray[2] = 'How';
$MyArray[3] = 'Are';
$MyArray[4] = 'You?';

echo $MyArray[2];
```

```
Dcl-S MyArray Char(5) Dim(10);

MyArray(1) = 'Hello';
MyArray(2) = 'World';
MyArray(3) = 'How';
MyArray(4) = 'Are';
MyArray(5) = 'You?';

dsply (MyArray(3));
```

***

### While loop
```php
$num = 0;

while ($num < 5) {
 echo 'number: ' . $num;
 $num++;
}
```

```
Dcl-S num Int(3) Inz(0);

Dow (num < 5);
  dsply ('number: ' + %Char(num));
  num += 1;
Enddo;
```

***

### For loop (Counting up example)
```php
for ($x = 0; $x <= 10; $x++) {
    echo "The number is: " . $x;
}
```

```
Dcl-S x Int(3);

For x = 0 to 10;
  dsply ('The number is: ' + %Char(x));
Endfor;
```

***

### For loop (Counting down example)
```php
for ($x = 10; $x >= 0; $x--) {
    echo "The number is: " . $x;
}
```

```
Dcl-S x Int(3);

For x = 10 downto 0;
  dsply ('The number is: ' + %Char(x));
Endfor;
```

***

### For loop (by amount)

* This example prints 0, 2, 4, 6, 8, 10
* The RPG `for` is also writable as `For x = 0 to 10 by 2`.

```php
for ($x = 0; $x <= 10; $x += 2) {
    echo "The number is: " . $x;
}
```

```
For x = 0 by 2 to 10;
  dsply ('The number is: ' + %Char(x));
Endfor;
```

***

### Function/procedure definition

* PHP passes parameters by value always.
* RPG passes by reference by default, but will pass by value with the `value` keyword.
* `*N` in RPG represents 'nothing' - you can optionally put the procedure name (`doDivide`) in its place.

```php
echo doDivide(10, 2);
return;

function doDivide($numA, $numB) {
    return $numA / $numB;
}
```

```
dsply (%Char(doDivide(10, 2)));
*InLR = *On;
Return;

Dcl-Proc doDivide;
  Dcl-Pi *N Int(3);
    numA Int(3) Value;
    numB Int(3) Value;
  End-Pi;
  
  Return numA / numB;
Edn-Proc;
```

***

### Test global scope (variable in global scope)

```php
$x = 5; // global scope
  
function myTest() {
     // using x inside this function will generate an error
     echo "<p>Variable x inside function is: $x</p>";
} 
myTest();

echo "<p>Variable x outside function is: $x</p>";
```

```
Dcl-S x Int(3) Inz(5); //Global scope

myTest();
dsply ('Variable x outside procedure is: ' + %Char(x));

*Inlr = *On;
Return;

Dcl-Proc myTest;
  // using x inside this function will be ok because it's global
  dsply ('Variable x inside procedure is: ' + %Char(x));
End-Proc;      
```

***

### Test local scope (variable inside function)
* This is a false program (won't compile) and script: using `x` outside the function/procedure will stop the compiler/script because x doesn't exist in the global scope.  

```php
function myTest() {
     $x = 5; // local scope
     echo "<p>Variable x inside function is: $x</p>";
} 
myTest();

// using x outside the function will generate an error
echo "<p>Variable x outside function is: $x</p>";
```

```
myTest();                                                          
dsply ('Variable x outside procedure is: ' + %Char(x)); 
                                                        
*Inlr = *On;                                            
Return;                                                 
                                                        
Dcl-Proc myTest;                                        
  Dcl-S x Int(3) Inz(5);                                
                                                        
  dsply ('Variable x inside procedure is: ' + %Char(x));
End-Proc;                                               
```

***

### Get the length of a string - %len / %trim
```php
echo strlen("Hello world!");
```

* If you pass in a variable to `%len`, it will return the length of the variable - this includes the blanks.

```
//We use %trim to remove the blanks from the character field
// Dcl-S someAlphaVar Char(100) Inz('Hello there world!');
// %len(%trim(someAlphaVar));

//%Char must be used because DSPLY won't except anything other than characters
dsply %char(%len('Hello world!'));
```

***

### Search for a specific text within a string - %Scan

* Both `strpos` and `%scan` return the starting index of the string.

```php
echo strpos("Hello world!", "world");
```

```
dsply %char(%scan('world':'Hello world!'));
```

***

### Replace text within a string - %scanrpl
```php
echo str_replace("world", "Barry", "Hello world!");
```

```
dsply %scanrpl('world':'Barry':'Hello world!');
```
