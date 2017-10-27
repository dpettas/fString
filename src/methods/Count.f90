
  Function       Count(This,Sub,Start,End) Result(SubCount)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Character(len=*), Intent(In) :: Sub
    Integer                      :: SubCount 
    Integer         , Optional   :: Start
    Integer         , Optional   :: End


    Integer                      :: SubLen
    Character(len=:),Allocatable :: DummyStr
    Integer                      :: i
    Integer                      :: Lc
    Integer                      :: Uc
    Integer                      :: Start_
    Integer                      :: End_

    Start_ = 1 
    End_   = len(This%Name)

    If (Present(Start)) Start_ = Start
    If (Present(End  )) End_   = End

    If (End_ > len(This%Name)) End_ = len(This%Name)

    If( End_ < Start_ ) Then
      SubCount = 0 
      Return
    End If



    DummyStr = This%Name(Start_:End_)
    SubLen   = len(Sub)
    SubCount = 0


    Do i = 1, len(DummyStr)

      Lc = i 
      Uc = Lc+SubLen-1
      If (Uc >= len(DummyStr)) Uc = len(DummyStr)

      if (DummyStr(Lc:Uc)==Sub) SubCount = SubCount + 1

    End Do
  End Function   Count

