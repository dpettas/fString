
Function     BooleanNotEqual_Str (This,L2) Result(Output)
   Implicit None
   Class(Str), Intent (In) :: This 
   Type (Str), Intent (In) :: L2 
   Logical                 :: Output


   Output = This%Name /= L2%Name
End Function BooleanNotEqual_Str

Function     BooleanNotEqual_Char (This,L2) Result(Output)
   Implicit None
   Class(Str)      , Intent (In) :: This 
   Character(len=*), Intent (In) :: L2 
   Logical                       :: Output


   Output = This%Name /= L2
End Function BooleanNotEqual_Char

   