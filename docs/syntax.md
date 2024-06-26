#

## Syntax

Some parts of Verox

### Operators

* **;**
* **{}**
* **()**
* **=**

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
* **else**

\* if can be chained after else like **else if**

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

int x = 3;
int y = 70;

bool z = y < x;
bool m = 6 < 7;

bool p = z;
int g = x;

if(!(z && m)) {
    return y;
}
else if(!m) {
    return 100;
}
else {
    return 105;
}
```
