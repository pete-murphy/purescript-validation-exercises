module Main where

import Prelude

import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Generic.Rep as Generic
import Data.Generic.Rep.Show as Generic.Show
import Data.Int (toNumber)
import Data.List.NonEmpty (NonEmptyList, fromFoldable, singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number (fromString)
import Data.String (length, null, toLower, toUpper)
import Data.Validation.Semigroup (V, invalid, unV)
import Effect (Effect)
import Effect.Console (log, logShow)
import Global.Unsafe (unsafeStringify)
import Text.Email.Validate as Email

type UnvalidatedForm =
  { emailAddress :: String
  , password    :: String
  , azimuth     :: String
  }

type ValidatedForm =
  { emailAddress :: EmailAddress
  , password    :: Password
  , azimuth     :: Azimuth
  }

validateForm
  :: UnvalidatedForm
  -> V (NonEmptyList InvalidField) ValidatedForm
validateForm { emailAddress, password, azimuth } =
  { emailAddress: _, password: _, azimuth: _ }
  <$> validateEmailAddress emailAddress
  <*> validatePassword password 8 60
  <*> validateAzimuth azimuth

------------------------------------------------------------------

main :: Effect Unit
main = do
  let validForm = { emailAddress: "alice@example.com", password: "GreatPassword", azimuth: "140" }
  let validFormTest = validateForm validForm
  logShow (printValidation validFormTest)
  
  log "\n"
  
  let invalidForm = { emailAddress: "example", password: "badpw", azimuth: "800" }
  let invalidFormTest = validateForm invalidForm
  logShow (printValidation invalidFormTest)
  
------------------------------------------------------------------
-- | Newtype wrapper for `String` indicating a valid email
newtype EmailAddress = EmailAddress String

-- | Newtype wrapper for `String` indicating a valid password
newtype Password = Password String

-- | Newtype wrapper for `Number` indicating a valid azimuth 
newtype Azimuth = Azimuth Number

-- | Type of validation errors encountered when validating form fields
data InvalidField
  = InvalidEmailAddress (NonEmptyList InvalidPrimitive)
 	| InvalidPassword     (NonEmptyList InvalidPrimitive)
 	| InvalidAzimuth      (NonEmptyList InvalidPrimitive)

validateEmailAddress 
  :: String 
  -> V (NonEmptyList InvalidField) EmailAddress
validateEmailAddress input =
      let result = 
               validateNonEmpty input
            *> validateEmail input
  	  in bimap (singleton <<< InvalidEmailAddress) EmailAddress result

validatePassword
  :: String
  -> Int
  -> Int
  -> V (NonEmptyList InvalidField) Password
validatePassword input minLength maxLength =
     let result =
              validateNonEmpty input
           *> validateContainsMixedCase input
           *> validateLength input minLength maxLength
     in bimap (singleton <<< InvalidPassword) Password result

validateAzimuth
  :: String
  -> V (NonEmptyList InvalidField) Azimuth
validateAzimuth input =
    let result = do
          input' <- validateIsNumber input
          validateInRange input' 0 360
    in bimap (singleton <<< InvalidAzimuth) Azimuth (fromEither result)

fromEither
  :: forall result err
  . Semigroup err
  => Either err result 
  -> V err result
fromEither (Left x)  = invalid x
fromEither (Right x) = pure x

--------------------------------------------------------------------------------
-- TYPES AND FUNCTIONS FOR PRIMITIVE VALIDATIONS

-- | Type of validation errors encountered when validating primitive input
data InvalidPrimitive 
  = EmptyField
  | InvalidEmail String
  | TooShort Int Int
  | TooLong Int Int
  | NoLowercase String
  | NoUppercase String
  | NotANumber String
  | NotInRange Int Int Number

-- | Validate that an input string can be parsed as number
validateIsNumber :: String -> Either (NonEmptyList InvalidPrimitive) Number
validateIsNumber input =
    case fromString input of
      Nothing -> Left (singleton (NotANumber input))
      Just n  -> pure n

-- | Validate that a number is within a range
validateInRange 
  :: Number
  -> Int 
  -> Int 
  -> Either (NonEmptyList InvalidPrimitive) Number
validateInRange input minValue maxValue =
  case (input >= toNumber minValue && input <= toNumber maxValue) of
    false -> Left (singleton (NotInRange minValue maxValue input))
    _     -> pure input

-- | Validate that an input string is not empty
validateNonEmpty :: String -> V (NonEmptyList InvalidPrimitive) String
validateNonEmpty input
  | null input = invalid (singleton EmptyField)
  | otherwise = pure input

-- | Wrapper around imported EmailAddress Validator
validateEmail 
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateEmail input =
  case Email.emailAddress input of
    Nothing -> invalid (singleton (InvalidEmail input))
    Just _  -> pure input

-- | Validate that an input string is at least as long as some given `Int`
validateMinimumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMinimumLength input minLength
  | (length input) < minLength = invalid (singleton (TooShort (length input) minLength))
  | otherwise = pure input
  
-- | Validate that an input string is shorter than given `Int`
validateMaximumLength
  :: String
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateMaximumLength input maxLength
  | (length input) > maxLength = invalid (singleton (TooLong (length input) maxLength))
  | otherwise = pure input
  
-- | Validate that an input string is within the minimum and maximum given `Int`
-- | lengths
validateLength
  :: String
  -> Int
  -> Int
  -> V (NonEmptyList InvalidPrimitive) String
validateLength input minLength maxLength =
     validateMinimumLength input minLength
  *> validateMaximumLength input maxLength

-- | Validate that an input string contains at least one lowercase character
validateContainsLowercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsLowercase input
  | (toUpper input) == input = invalid (singleton (NoLowercase input))
  | otherwise = pure input
  
-- | Validate that an input string contains at least one uppercase character
validateContainsUppercase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsUppercase input
  | (toLower input) == input = invalid (singleton (NoUppercase input))
  | otherwise = pure input
  
-- | Validate that an input string contains some mix of upper- and lowercase
-- | characters
validateContainsMixedCase
  :: String
  -> V (NonEmptyList InvalidPrimitive) String
validateContainsMixedCase input =
     validateContainsLowercase input
  *> validateContainsUppercase input

---------------------------------------------------------------------------------
-- !!! BOILERPLATE TO MAKE EVERYTHING A LITTLE EASIER TO WORK WITH            !!!
---------------------------------------------------------------------------------

-- | Helper function to print validations
printValidation 
  :: forall err result
   . Show err
  => V (NonEmptyList err) result
  -> String
printValidation = 
  unV (show <<< fromFoldable) (\result -> "Valid: " <> (unsafeStringify result))

-- | Derive a `Generic` instance for `InvalidPrimitive` so we can get a `Show` 
-- | instance to print to the console.
derive instance genericInvalidPrimitive :: Generic.Generic InvalidPrimitive _

-- | Derive `show` for `InvalidPrimitive` using the `Generic` instance.
instance showInvalidPrimitive :: Show InvalidPrimitive where
  show = Generic.Show.genericShow
  
-- | Manually derive a `Show` instance for `EmailAddress` so it prints nicely
derive instance newtypeEmailAddress :: Newtype EmailAddress _
instance showEmailAddress :: Show EmailAddress where 
  show = unwrap

-- | Manually derive a `Show` instance for `Password` so it prints nicely
derive instance newtypePassword :: Newtype Password _
instance showPassword :: Show Password where
  show = unwrap

-- | Manually derive a `Show` instance for `Azimuth` so it prints nicely
derive instance newtypeAzimuth :: Newtype Azimuth _
instance showAzimuth :: Show Azimuth where
  show = show <<< unwrap

-- | Manually derive a `Show` instance for `InvalidField` that pretty prints the
-- | `NonEmptyList`s as `Array`s
instance showInvalidField :: Show InvalidField where
  show = case _ of
    InvalidEmailAddress errs -> "(InvalidEmailAddress " <> (show (fromFoldable errs)) <> ")"
    InvalidPassword     errs -> "(InvalidPassword "     <> (show (fromFoldable errs)) <> ")"
    InvalidAzimuth      errs -> "(InvalidAzimuth "      <> (show (fromFoldable errs)) <> ")"