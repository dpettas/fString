


  
  Function       Add_Left_Char_Right_Char(Char1,Char2) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Char1
    Character(len=*), Intent(In) :: Char2
    Type(Str)                    :: OutPut    

    OutPut = Char1//Char2
  End Function   Add_Left_Char_Right_Char

  Function       Add_Left_Str_Right_Char (This,Char) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Character(len=*), Intent(In) :: Char
    Type(Str)                    :: OutPut    



    OutPut%Name = This%Name//Char
  End Function   Add_Left_Str_Right_Char

  Function       Add_Left_Char_Right_Str (Char,This) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Character(len=*), Intent(In) :: Char
    Type(Str)                    :: OutPut    


    OutPut%Name = Char//This%Name
  End Function   Add_Left_Char_Right_Str


  Function       Add_Left_Str_Right_Str (This,Str2) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Type (Str)      , Intent(In) :: Str2
    Type (Str)                   :: OutPut    


    OutPut%Name = This%Name + Str2%Name
  End Function   Add_Left_Str_Right_Str

