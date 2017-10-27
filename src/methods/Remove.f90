
  Function Remove_Char (This,Char) Result(OutPut)
    Implicit None 
    Class(Str)                   :: This
    Type (Str)                   :: OutPut
    Character(len=*), Intent(In) :: Char


    Integer                      :: Pos = -10
    Integer                      :: i
    Integer                      :: LenChar
    Type(Str)                    :: Temp



    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    !   Check Cases
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    If (Char              == This%Name) Then ; OutPut = ''   ; Return 
    End If 

    If (Char              == ''       ) Then ; OutPut = This ; Return 
    End If

    If (This%Name         == ''       ) Then ; OutPut = ''   ; Return 
    End If 

    If (This%FindOne(Char)== 0        ) Then ; OutPut = This ; Return 
    End If 

    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! Remove Algorithm
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    Temp    = This
    LenChar = Len(Char) 

    Pos  = Temp%FindOne(Char)
    Do While (Pos/=0)
    Temp = Temp%Name(1:Pos-1) + Temp%Name(Pos+LenChar:Len(Temp))
    Pos  = Temp%FindOne(Char)
    End Do 


    OutPut = Temp
  End Function Remove_Char




  Function Remove_Str (This,Char) Result(OutPut)
    Implicit None 
    Class(Str)                   :: This
    Type (Str)                   :: OutPut
    Type (Str),       Intent(In) :: Char


    Integer                      :: Pos = -10
    Integer                      :: i
    Integer                      :: LenChar
    Type(Str)                    :: Temp

    OutPut = This%Remove_Char(Char%Name)
  End Function Remove_Str
