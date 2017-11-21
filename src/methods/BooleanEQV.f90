
Function     BooleanEqual_Str (This,L2) Result(Output)
   Implicit None
   Class(Str), Intent (In) :: This 
   Type (Str), Intent (In) :: L2 
   Logical                 :: Output


   Output = This%Name == L2%Name
End Function BooleanEqual_Str

Function     BooleanEqual_Char (This,L2) Result(Output)
   Implicit None
   Class(Str)      , Intent (In) :: This 
   Character(len=*), Intent (In) :: L2 
   Logical                       :: Output


   Output = This%Name == L2
End Function BooleanEqual_Char

   