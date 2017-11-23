!################################################################################################
! The Main Purpose of this module is to use the Object Oriented forrm of the Fortran language
! This Module is offers to the User The right way in order to Use the Strings in Fortran
!
!################################################################################################
!
! The Module String Contains a Character in Allocatable forrm which is called Name while all the 
! others are the Methods of the Class.
!
!
!################################################################################################
! If You Want to Print for Example theCharacter Name you 
! write 
! Print*, <Your Object type of Str>%Name
!
! Methods of the Class:
!------------------------------------------------------------------------------------------------
!                                            OVERLOAD OPERATORS
!------------------------------------------------------------------------------------------------
! ** Assignment(=) :: 
!
!   <Type Str> = <Type Str>        (Copy an object to the Other)
!   <Type Str> = Character(len=*)  (This is Basically the Constructor of the Str 
!                                   is equal to Str%Name = Character(len=*)     )
!   Character(len=*) = <Type Str>  (Copy the Str%Name to a  Character)
!   Real(8)          = <Type Str>  (Convert an Array to Real. If Str is not a number the Real(8)=NaN)
!   Int              = <Type Str>  (Convert an Array to Int. If Str is not a number Int=NaN)
!
! ** Multiply (*)  ::
!
! <Type Str> = <Type Str> * Int or <Type Str> = Int * <Type Str>
!                                   (This Module multiplies Int Times the string Str)
!
! ** Add (+)       ::
! <Type Str> = <Type Str> + <Type Str>
! <Type Str> = Character  + <Type Str>
! <Type Str> = <Type Str> + Character
! <Type Str> = Character  + Character
!
! Merges a two Strings or Characters to one object Str
!
!------------------------------------------------------------------------------------------------
!                                                 METHODS
!------------------------------------------------------------------------------------------------
!
! **<Type Str> = UpperCase :: Converts all the String Character to Upper Case (Return <Type Str>)
! **<Type Str> = LowerCase :: Converts all the String Character to Lower Case (Return <Type Str>)
! **<Type Str> = Reverse   :: Reverse the String. Optional(Start (Character to start with ), End Character End With)
!                             By Default Start = 1 and End = len(Str)
!
!
! **<Type Str> = Replace(Old, New, Optional Max)
!
!    Replace the Substring Old with the Substring New inside the Str Max Times
! 
!     By Default if Max is absent then all the Substrings Old in the Str will be replaced
!
!
! **<Type Str> = CutSpaces
! 
!     This Trim the Spaces At the begging and the End of the Character
!
! **<Type Str Dimension Allocatable> :: Split(Optional Sep)
!
!   This Splits the String into SubStrings that are separated by the Sep
!   By Default the Sep = " " (One Space)
!
! **<Logical> = IsDigit         :: True If the String Contains Only Numbers False Otherwise
! **<Logical> = IsAlpha         :: The opposite of IsDigit
! **<Logical> = IsNumeric       :: True If the String Is A Number
! **<Logical> = StartsWith(Sub) :: Returns True if the String Starts with the Substring Sub
!
! **<Integer> = Count      :: Counts the Total appearance of a Substring in a String
!                             (Optional Start, End in which by default is equal to 1, len(Str))
!
! **<Integer> = FindOne    :: Finds The Position in the String of a Substring (Returns the First Index)
!                             Optional Start, End
!
!
! **<Integer Dimension Allocatable> = Find :: Finds All the Positions in the String of a Substring 
!                                             (Returns the First Index of Each One)
!                                             By Default Start = 1, End = len(Str)
! **
!################################################################################################

#ifndef FSTRING
#define FSTRING



Module fString

  Public  


!  Private ::  SetName      , UpperCase_Def, LowerCase, Count, Reverse   , IsDigit, IsAlpha     , &
!
!              IsNumeric    , Str2Real8    , Str2Int  , Find , StartsWith, Replace, Str2Char_Sub, &
!              Str2Real8_Sub, Str2Int_Sub  , CutSpaces                                          , &

!              Multiplicity_Right_Int                                                           , &
!              Multiplicity_Left_Int                                                            , &
!              Add_Left_Str_Right_Char                                                          , &
!              Add_Left_Char_Right_Str                                                          , &
!              Add_Left_Char_Right_Char                                                         , &
!              Len_Str

  !************************************************************************************************
  !   
  !                                  DEFINE INTERFACES
  !
  !************************************************************************************************


  Interface  Operator(+)
    Module Procedure :: Left_Char_Right_Str_Addition
    Module Procedure :: Left_Char_Right_Real8
    Module Procedure :: Left_Char_Right_Integer
  End Interface

  Interface Len 
    Module Procedure :: Len_Str
  End Interface




  !************************************************************************************************
  !
  !                                           CLASS Str
  !
  !************************************************************************************************



  Type     Str

    Character(len=:), Allocatable              :: Name
    
    Contains

    Procedure                      :: UpperCase     => UpperCase_Def
    Procedure                      :: LowerCase
    Procedure                      :: Count
    Procedure                      :: Reverse
    Procedure                      :: IsDigit
    Procedure                      :: IsAlpha
    Procedure                      :: IsNumeric
    Procedure                      :: Real8         => Str2Real8
    Procedure                      :: Int           => Str2Int
    Procedure                      :: FindOne
    Procedure                      :: Find          => FindAll
    Procedure                      :: StartsWith
    Procedure                      :: EndsWith
    Procedure                      :: Replace
    Procedure                      :: CutSpaces
    Procedure                      :: Split
    Procedure                      :: Colorize
    Procedure                      :: WriteStrUDIO
    Procedure                      :: ReadStrUDIO


    Procedure, Private, Pass(This) :: FindPattern_Str
    Procedure, Private, Pass(This) :: FindPattern_Char

    Procedure, Private, Pass(This) :: Remove_Char
    Procedure, Private, Pass(This) :: Remove_Str
    Procedure, Private, Pass(This) :: SetName
    Procedure, Private, Pass(This) :: Str2Char_Sub
    Procedure, Private, Pass(This) :: Str2Real8_Sub
    Procedure, Private, Pass(This) :: Str2Int_Sub
    Procedure, Private, Pass(This) :: Int2Str_Sub
    Procedure, Private, Pass(This) :: Double2Str_Sub


    Procedure, Private, Pass(This) :: Multiplicity_Right_Int
    Procedure, Private, Pass(This) :: Multiplicity_Left_Int
    Procedure, Private, Pass(This) :: Add_Left_Str_Right_Char
    Procedure, Private, Pass(This) :: Add_Left_Char_Right_Str
    Procedure, Private, Pass(This) :: Add_Left_Str_Right_Str

    Procedure, Private, Pass(This) :: Left_Str_Right_Real8
    Procedure, Private, Pass(This) :: Left_Str_Right_Integer
    Procedure, Private, Pass(This) :: Left_Str_Right_Str_Addition

    Procedure, Private, Pass(This) :: BooleanEqual_Str
    Procedure, Private, Pass(This) :: BooleanEqual_Char
    Procedure, Private, Pass(This) :: BooleanNotEqual_Str
    Procedure, Private, Pass(This) :: BooleanNotEqual_Char



    Procedure                      :: Help

    Generic                        :: Remove           => Remove_Char, Remove_Str
    Generic                        :: Pattern          => FindPattern_Str          , &
                                                          FindPattern_Char
    Generic                        :: Assignment(=)    => SetName                  , &
                                                          Str2Real8_Sub            , &
                                                          Str2Int_Sub              , &
                                                          Str2Char_Sub             , &
                                                          Int2Str_Sub              , &
                                                          Double2Str_Sub

                                                          
    Generic                        :: Operator  (*)    => Multiplicity_Right_Int   , &
                                                          Multiplicity_Left_Int

    Generic                        :: Operator  (//)   => Add_Left_Str_Right_Char  , &
                                                          Add_Left_Char_Right_Str  , &
                                                          Add_Left_Str_Right_Str   


    Generic                        :: Operator  (+)    => Left_Str_Right_Real8       , &
                                                          Left_Str_Right_Integer     , &
                                                          Left_Str_Right_Str_Addition

    Generic                        :: Operator (==)    => BooleanEqual_Str   , BooleanEqual_Char
    Generic                        :: Operator (/=)    => BooleanNotEqual_Str, BooleanNotEqual_Char


    Generic                        :: Write(Formatted) =>  WriteStrUDIO                                                     
    Generic                        :: Read (Formatted) =>  ReadStrUDIO
  End Type Str

  !************************************************************************************************
  !************************************************************************************************


  Character( * ), Parameter, Private :: LOWER_CASE      = 'abcdefghijklmnopqrstuvwxyz'
  Character( * ), Parameter, Private :: UPPER_CASE      = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  Character( * ), Parameter, Private :: DefaultFormat   = 'F10.4'
  Character( * ), Parameter, Private :: DefaultIntFmt   = 'i10'
  Integer       , Parameter, Private :: DefaultLen      = 1000
  Character( * ), Parameter, Private :: TestWork        = '244ce1fc9a52932f9c06d534a8f4d733' !(MD5 format)


  Contains 

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  !                           Include Files
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>


  Subroutine Help(This,Unit,Search)
    Implicit None 
    Class(Str)                               :: This
    Integer         , Intent(In), Optional   :: Unit
    Character(len=*), Intent(In), Optional   :: Search

    Integer                                  :: Unit_

    ! Methods
    Integer  , Parameter                     :: NumMethod = 21
    Type(Str), Dimension(NumMethod)          :: Method_
    Type(Str)                                :: MethodDummy

    Type(Str)                                :: MethodName
    Integer                                  :: I
    Logical                                  :: MethodExist
    


    If  ( Present(Unit) ) Then ; Unit_ = Unit
    Else                       ; Unit_ = 6 
    End If 

    MethodExist = Present(Search)

    If (MethodExist) Then ; MethodName = Search ; MethodName = MethodName%LowerCase()
    End If 

    I = 0
    I = I + 1 ;Method_(I)  =  "(Function)  UpperCase  : <Type Str>               = <Type Str>%UpperCase   ()"
    I = I + 1 ;Method_(I)  =  "(Function)  LowerCase  : <Type Str>               = <Type Str>%LowerCase   ()"
    I = I + 1 ;Method_(I)  =  "(Function)  Count      : <Integer >               = <Type Str>%Count       (Sub (Character len(*)),Start (Integer,Optional), End (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  Reverse    : <Type Str>               = <Type Str>%Reverse     ()"
    I = I + 1 ;Method_(I)  =  "(Function)  IsDigit    : <Logical >               = <Type Str>%IsDigit     ()"
    I = I + 1 ;Method_(I)  =  "(Function)  IsAlpha    : <Logical >               = <Type Str>%IsAlpha     ()"
    I = I + 1 ;Method_(I)  =  "(Function)  IsNumeric  : <Logical >               = <Type Str>%IsNumeric   ()"
    I = I + 1 ;Method_(I)  =  "(Function)  StartsWith : <Logical >               = <Type Str>%StartsWith  (Sub (Character len(*)),Start (Integer,Optional), End (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  EndsWith   : <Logical >               = <Type Str>%EndsWith    (Sub (Character len(*)),Start (Integer,Optional), End (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  FindOne    : <Integer >               = <Type Str>%FindOne     (Sub (Character len(*)),Start (Integer,Optional), End (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  Find       : <Integer >               = <Type Str>%Find        (Sub (Character len(*)),Start (Integer,Optional), End (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  Replace    : <Type Str>               = <Type Str>%Replace     (Old (Character len(*)),New   (Character len(*)), Max (Integer,Optional))"
    I = I + 1 ;Method_(I)  =  "(Function)  CutSpaces  : <Type Str>               = <Type Str>%CutSpaces   ()"
    I = I + 1 ;Method_(I)  =  "(Function)  Split      : <Type Str, Dimension(:)> = <Type Str>%Split       (Sep     (Character len(*)))             "
    I = I + 1 ;Method_(I)  =  "(Function)  Pattern    : <Type Str, Dimension(:)> = <Type Str>%Pattern     (Pattern (Character len(*) or Type Str ))"
    I = I + 1 ;Method_(I)  =  "(Function)  Remove     : <Type Str>               = <Type Str>%Remove      (Char    (Character len(*) or Type Str)))"

    I = I + 1 ;Method_(I)  =  "(Operator) <Type Str> =  <Integer>  * <Type Str> or  <Type Str> *  <Integer>"
    I = I + 1 ;Method_(I)  =  "(Operator) <Type Str> =  <Type Str or Character> + <Type Str> or  <Real(8)> or  <Integer> or <Type Str>"
    I = I + 1 ;Method_(I)  =  "(Operator) <Type Str> =  <Type Str or Character> // <Type Str or Character>"
    I = I + 1 ;Method_(I)  =  "(Operator) <Type Str> == <Type Str>              or  <Type Str> == Char(len=*)"


!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!   Loop Over the Methods 
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    Do i = 1, NumMethod

    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! If the Unit == 6 (print) then Colorize the reusults else 
    ! leave it white
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    If (Unit_== 6) Then 
    Method_(i) = Method_(i)%Colorize("(Function)","Red   Bold")
    Method_(i) = Method_(i)%Colorize("(Operator)","Green Bold")
    Method_(i) = Method_(i)%Colorize("Optional"  ,"      Bold")
    End If 

    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ! If Search exists then 
    !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

    MethodDummy = Method_(i)%LowerCase()
    If (MethodExist ) Then
      

      If (MethodDummy%FindOne( MethodName%Name ) /= 0 ) Then 
      Write(Unit_,'(dt)') Method_(i)
      End If 
      
    Else
    Write(Unit_,'(dt)') Method_(i)
    End If 
    
    
    

    End Do 

  End Subroutine Help 





# include "./methods/SetName.f90"

# include "./methods/UpperCase.f90"
# include "./methods/LowerCase.f90"
# include "./methods/Addition.f90"
# include "./methods/Count.f90"
# include "./methods/Multiplicity.f90"
# include "./methods/Split.f90"
# include "./methods/Find.f90"
# include "./methods/IsDigit.f90"
# include "./methods/IsNumeric.f90"
# include "./methods/IsAlpha.f90"
# include "./methods/Replace.f90"
# include "./methods/StartsWith.f90"
# include "./methods/EndsWith.f90"
# include "./methods/Reverse.f90"
# include "./methods/Convert.f90"
# include "./methods/CutSpaces.f90"
# include "./methods/Len_Str.f90"
# include "./methods/FindPattern.f90"
# include "./methods/Colorize.f90"
# include "./methods/Remove.f90"
# include "./methods/Read.f90"
# include "./methods/Write.f90"
# include "./methods/BooleanEQV.f90"
# include "./methods/BooleanNEQV.f90"


End Module fString



#endif
