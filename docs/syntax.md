#

## Syntax

Some parts of Verox

### Operators

* **;**
* **{**
* **}**
* **(**
* **)**

### Binary operators

* **+**
* **-**
* **\***
* **/**

### Logic operators

* **&&**
* **||**
* **!**

### Comparison operators

* **==**
* **>**
* **>=**
* **<**
* **<=**
* **!=**

### Keywords

* **return**
* **if**

#### Types

* **int** = an unsigned 64-bit integer.
* **bool** = a boolean value stored as a 64 bit integer (nice and wasteful).

<!--### Built-in functions
 * **void print(int n)** = will put a raw int to the output console and append newline character. -->

### Example syntaxes

The following is a simple program using most features available.

```cpp
{
    int y = 2;
    int x = 1;
    int z = 3;
    bool b = y == x;
    bool c = z < y;
    bool a = x >= z;
}

int y = 10 * (2 + 3);
int x = 7;
int z = y - x;

if(z < y) { return x;}
return z;
```
