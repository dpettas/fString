
  Function       FindOne(This,Sub,Start,End) Result(OutPut)
    Implicit None
    Class(Str)                                 , Intent(In) :: This 
    Character(Len=*)                           , Intent(In) :: Sub
    Integer         , Optional                 , Intent(In) :: Start
    Integer         , Optional                 , Intent(In) :: End

    Integer                                                 :: OutPut 

    Character(len=1000)                                     :: String
    Integer                                                 :: Start_
    Integer                                                 :: End_

    Start_ = 1
    End_   = len(This)

    If (Present(Start)  ) Start_ = Start
    If (Present(End  )  ) End_   = End
    If (End_ > len(This)) End_   = len(This)

    If (End_ < Start_   ) Then 
    OutPut = -1
    Return
    End If 

    String = This%Name(Start_:End_)
    If (Present(Start)) Then ; OutPut = Start_ + Index(String             , Sub) - 1
    Else                     ; OutPut =          Index(String(Start_:End_), Sub) 
    End If
  End Function   FindOne



  Function       FindAll(This,Sub,Start,End) Result(OutPut)
    Implicit None
    Class(Str)                                 , Intent(In) :: This 
    Character(Len=*)                           , Intent(In) :: Sub
    Integer         , Optional                 , Intent(In) :: Start
    Integer         , Optional                 , Intent(In) :: End

    Integer, Dimension(:), Allocatable                      :: OutPut 
    Integer                                                 :: NumSubStrings
    Integer                                                 :: ISub
    Integer                                                 :: Iindex
  
  
  
    
    NumSubStrings = This%Count(Sub)
    If (Allocated(OutPut))       DeAllocate(OutPut)
    If (NumSubStrings>0)  Then ;   Allocate(OutPut(NumSubStrings)) 
    Else                       ;   Allocate(OutPut(1)); OutPut = -1 ; Return 
    End If
    

    OutPut = -1
    Iindex    = 0
    OutPut(1) = 1

    Do ISub = 1, NumSubStrings
      OutPut(ISub) = This%FindOne(Sub,Start=Iindex+1)
      Iindex       = OutPut(ISub)
    End Do


  End Function   FindAll

