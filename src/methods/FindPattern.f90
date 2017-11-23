
  Function       FindPattern_Char(This,Pattern) Result(OutPut)
    Implicit None 
    Class(Str)      , Intent(In)               :: This
    Character(len=*), Intent(In)               :: Pattern
    Type (Str)                                 :: Work
    Type (Str)      , Dimension(:),Allocatable :: OutPut

    Work = Pattern

    OutPut = This%FindPattern_Str(Work)
  End Function   FindPattern_Char



  Function       FindPattern_Str(This,Pattern) Result(OutPut)
    Implicit None 
    Class(Str), Intent(In)                            :: This
    Type (Str), Intent(In)                            :: Pattern
    Type (Str), Dimension(:),Allocatable              :: OutPut

    Type (Str)                                        :: Working
    Type (Str)                                        :: WorkingPattern
    Integer                                           :: i
    Integer                                           :: L
    Integer                                           :: U
    Integer                                           :: BraKet    
    Integer                                           :: LenBraKet
    Integer  , Dimension(:), Allocatable              :: PP 
    Type(Str), Dimension(:), Allocatable              :: Sub

    ! I found a bug in the FindPattern_Str Could not analyze the Pattern like 
    ! "Abc Test1 def Test2" where Pattern = "Abc {} def {}" where {} are at the end 
    ! of the string so I add the TestWork sting which is the Word TestWork at MD5 format

    Working        = This    // TestWork
    WorkingPattern = Pattern // TestWork


    BraKet    = WorkingPattern%Count("{}")
    LenBraKet = len("{}") 


    Allocate(PP  (BraKet  )) ; PP      = 0 ! Temporary Solution
    Allocate(Sub (BraKet+1))

    PP  = WorkingPattern%Find  ("{}")  ! Find The Position of BraKet



    Do i = 1, Size(Sub)

      If  (i==1) Then        ; L = 1
      Else                   ; L = PP(i-1) + LenBraKet 
      End if

      If (i==Size(Sub)) Then ; U = len(WorkingPattern)
      Else                   ; U = PP(i)-1
      End If


      Sub(i)= WorkingPattern%Name(L:U)

    End Do


    If (Allocated(OutPut)) DeAllocate(OutPut)
    Allocate(OutPut(Size(Sub)-1))



    Do i = 1, Size(Sub)-1
      If   (i==1) Then ; L =                         1 + len(Sub(i))
      Else             ; L = Working%FindOne(Sub(i)%Name) + len(Sub(i))
      End If 

      U = Working%FindOne(Sub(i+1)%Name) - 1

      OutPut(i) = Working%Name(L:U)

    End Do 
  End Function   FindPattern_Str
  



  Function       Left_Str_Right_Str_Addition (This,Value) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Type (Str)      , Intent(In) :: Value
    Type (Str)                   :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = This


    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))


    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Value%Name,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Value%Name,1)
    End  If
    
    OutPut = Work
  End Function   Left_Str_Right_Str_Addition


  Function       Left_Char_Right_Str_Addition (Ch,Value) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Ch
    Type (Str)      , Intent(In) :: Value
    Type (Str)                   :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = Ch


    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))


    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Value%Name,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Value%Name,1)
    End  If
    
    OutPut = Work
  End Function   Left_Char_Right_Str_Addition



















  Function       Left_Str_Right_Real8 (This,Value) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Real(8)         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = This
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))

    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval 
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval 
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval 
    End   If 


    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
    

    OutPut = Work
  End Function   Left_Str_Right_Real8

  Function       Left_Str_Right_Integer (This,Value) Result(OutPut)
    Implicit None
    Class(Str)      , Intent(In) :: This
    Integer         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Integer                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = This
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))

    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
  

    OutPut = Work
  End Function   Left_Str_Right_Integer


  
  Function       Left_Char_Right_Real8 (Ch,Value) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Ch
    Real(8)         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Real(8)                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = Ch
    Wval = Value

    Bra  = Work%FindOne("{")
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))



    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultFormat//')') Wval
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 )  Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 )  Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
      

    OutPut = Work
  End Function   Left_Char_Right_Real8


  Function       Left_Char_Right_Integer (Ch,Value) Result(OutPut)
    Implicit None
    Character(len=*), Intent(In) :: Ch
    Integer         , Intent(In) :: Value
    Type(Str)                    :: OutPut    

    Type(Str)                    :: Work
    Character(len=DefaultLen)    :: Wnum = ''
    Integer                      :: Wval
    Integer                      :: Bra
    Integer                      :: ket
    Character(len=:),allocatable :: Fmt
    Character(len=:),allocatable :: Num



    Work = Ch
    Wval = Value

    Bra  = Work%FindOne("{")  
    ket  = Work%FindOne("}")

    Fmt  = Trim(Adjustl(Work%Name(Bra+1:Ket-1)))



    If       (ket == Bra + 1 ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval 
    Else  If (Ket >  Bra + 1 ) Then  ; Write(Wnum,'('//Fmt          //')') Wval 
    Else  If (fmt == ''      ) Then  ; Write(Wnum,'('//DefaultIntFmt//')') Wval 
    End   If 



    Num  = Trim(Adjustl(Wnum))

    If      (ket == Bra + 1 ) Then  ; Work = Work%Replace("{}"              ,Num,1)
    Else If (Ket >  Bra + 1 ) Then  ; Work = Work%Replace(Work%Name(Bra:Ket),Num,1)
    End  If
    

    OutPut = Work
  End Function   Left_Char_Right_Integer