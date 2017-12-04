
# fString




- fString is a pure Object-Oriented fortran library that provides String objects 
- fString is Fortran 2008+ standard compliant
- fString is OOP designed
- fString is a free Open Source Library 

# Why use fString

Modern Fortran has introduced the Object-Oriented Standard. Using these type of Derived types as long as the User-defined Input Output it is possible to create classes to integrate the usage of the Fortran. In the Fortran, by default, there is a lack of classes to reduce the dummy code that we are using. fString is an integration of Characters, which are replaced with the Type(Str). Basically, Type(Str) is a Fortran Alpharithmetic equipped with some methods for the better usage of these Strings.

# Methods 


+ [x] [UpperCase](https://github.com/dpettas/fString/wiki/UpperCase-LowerCase) 
+ [x] [LowerCase](https://github.com/dpettas/fString/wiki/UpperCase-LowerCase)
+ [x] [Reverse](https://github.com/dpettas/fString/wiki/Reverse)
+ [x] [Replace](https://github.com/dpettas/fString/wiki/Replace)
+ [x] Count
+ [x] IsDigit
+ [x] IsAlpha
+ [x] IsNumeric
+ [x] StartsWith
+ [x] EndsWith 
+ [x] Find
+ [x] CutSpaces
+ [x] Split
+ [x] Pattern
+ [x] Remove

# Operators
+ Multiply Integer with Type(Str)
+ Addition a Type(Str) with Real number or Integer and vice versa
+ Assignment a Type(Str) with a Real number of Integer and vice versa
+ Merge a Type(Str) with a Character or another Type(Str) (//)
+ Boolean equilivalent (==) of a Type(Str) or Character

# How to use fString

```fortran
Program Main 
  Use fString
  Implicit None 
  Type(Str) :: MyStr  
  
End Program Main 
```
