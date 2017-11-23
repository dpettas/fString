

  Function       StartsWith(This,Sub,Start,End) Result(OutPut)
    Implicit None 
    Class(Str)                                 , Intent(In) :: This
    Character(len=*)                           , Intent(In) :: Sub
    Integer         , Optional                 , Intent(In) :: Start
    Integer         , Optional                 , Intent(In) :: End

    Integer                                                 :: Start_
    Integer                                                 :: End_
    Logical                                                 :: OutPut
    Integer                                                 :: LenSub


    LenSub = len(Sub)
    Start_ = 1 
    End_   = len(Sub)

    If (Present(Start)         ) Start_ = Start
    If (Present(End  )         ) End_   = End

    If ( End_- Start_ > LenSub ) End_   = Start_ + LenSub

    OutPut= This%Name(Start_:End_)==Sub
  End Function   StartsWith

