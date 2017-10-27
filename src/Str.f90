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
    Module Procedure :: Add_Left_Char_Right_Char 
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
    
    Procedure, Private             :: FindPattern_Str
    Procedure, Private             :: FindPattern_Char

    Procedure, Private, Pass(This) :: Remove_Char
    Procedure, Private, Pass(This) :: Remove_Str
    Procedure, Private, Pass(This) :: SetName
    Procedure, Private, Pass(This) :: Str2Char_Sub
    Procedure, Private, Pass(This) :: Str2Real8_Sub
    Procedure, Private, Pass(This) :: Str2Int_Sub
    Procedure, Private, Pass(This) :: Multiplicity_Right_Int
    Procedure, Private, Pass(This) :: Multiplicity_Left_Int
    Procedure, Private, Pass(This) :: Add_Left_Str_Right_Char
    Procedure, Private, Pass(This) :: Add_Left_Char_Right_Str
    Procedure, Private, Pass(This) :: Add_Left_Str_Right_Str
    Procedure, Private, Pass(This) :: Left_Str_Right_Real8
    Procedure, Private, Pass(This) :: Left_Str_Right_Integer




    ! I/O Procedure Only for Ifort Up to Now 
    ! User Defined Derived Types I/O (U.D. I/O)
    ! Procedure                      :: Write_Str_UDIO
    Generic                        :: Remove        => Remove_Char, Remove_Str

    Generic                        :: Pattern       => FindPattern_Str          , &
                                                       FindPattern_Char

    Generic                        :: Assignment(=) => SetName                  , &
                                                       Str2Real8_Sub            , &
                                                       Str2Int_Sub              , &
                                                       Str2Char_Sub
    Generic                        :: Operator  (*) => Multiplicity_Right_Int   , &
                                                       Multiplicity_Left_Int
 
    Generic                        :: Operator  (+) => Add_Left_Str_Right_Char  , &
                                                       Add_Left_Char_Right_Str  , &
                                                       Add_Left_Str_Right_Str   , &
                                                       Left_Str_Right_Real8     , &
                                                       Left_Str_Right_Integer



    ! Generic                        :: Write(Formatted) =>  Write_Str_UDIO                                                     
  End Type Str

  !************************************************************************************************
  !************************************************************************************************


  Character( * ), Parameter, Private :: LOWER_CASE      = 'abcdefghijklmnopqrstuvwxyz'
  Character( * ), Parameter, Private :: UPPER_CASE      = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  Character( * ), Parameter, Private :: DefaultFormat   = 'F10.4'
  Character( * ), Parameter, Private :: DefaultIntFmt   = 'i10'
  Integer       , Parameter, Private :: DefaultLen      = 1000


  Contains 

  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  !                           Include Files
  !<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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
# include "./methods/Remove.f90"

End Module fString



#endif
