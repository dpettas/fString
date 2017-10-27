
  Function       EndsWith(This,Sub,Start,End) Result(OutPut)
    Implicit None 
    Class(Str)                                 , Intent(In) :: This
    Character(len=*)                           , Intent(In) :: Sub
    Integer         , Optional                 , Intent(In) :: Start
    Integer         , Optional                 , Intent(In) :: End

    Type(Str)                                               :: RevThis
    Integer                                                 :: Start_
    Integer                                                 :: End_
    Logical                                                 :: OutPut
    Integer                                                 :: LenSub
    Type(Str)                                               :: RSub



    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! Reverse The Substring
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    RSub = Sub ; RSub = RSub%Reverse()

    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! Define The Optional Values
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    Start_ = 1        ; If (Present(Start)         ) Start_ = Start
    End_   = len(RSub); If (Present(End  )         ) End_   = End

    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! Reverse This Str
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    RevThis = This%Reverse()

    
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! Use StartsWith In the Reverse String In order To define the EndsWith
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    OutPut  = RevThis%StartsWith(RSub%Name,Start_,End_)
  End Function   EndsWith

