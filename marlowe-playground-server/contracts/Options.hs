{-# LANGUAGE OverloadedStrings #-}
module Options where

import Language.Marlowe.Extended

-- Options

data OptionType =
      Call -- ^ Call, i.e. the right to buy the Option
    | Put  -- ^ Put, i.e. the right to sell the Option
  deriving Show

data ExerciseType =
      European -- ^ Execution at expiry
    | American -- ^ Execution anytime before expiry
  deriving Show

-- |European style Options
european ::
     OptionType -- ^ Type of option
  -> Party      -- ^ Buyer
  -> Party      -- ^ Seller
  -> Token      -- ^ Currency
  -> Token      -- ^ Underlying
  -> Value      -- ^ Ratio
  -> Value      -- ^ Strike
  -> Timeout    -- ^ Expiry
  -> Contract   -- ^ Option contract
european optionType buyer seller currency underlying ratio strike expiry =
    When [] expiry
  $ american optionType buyer seller currency underlying ratio strike expiry

-- |American style Options
american ::
     OptionType -- ^ Type of option
  -> Party      -- ^ Buyer
  -> Party      -- ^ Seller
  -> Token      -- ^ Currency
  -> Token      -- ^ Underlying
  -> Value      -- ^ Ratio
  -> Value      -- ^ Strike
  -> Timeout    -- ^ Expiry
  -> Contract   -- ^ Option contract
american optionType buyer seller currency underlying ratio strike expiry =
    When
      [ Case
          (Choice choiceId [Bound 0 1])
          (If (ValueEQ (ChoiceValue choiceId) (Constant 0)) Close (transfer optionType buyer seller))
      ] expiry
    Close
  where
    choiceId =
      ChoiceId
        (label optionType)
        buyer
    label Call = "Exercise Call Option"
    label Put  = "Exercise Put Option"
    transfer Call a b =
        deposit currency strike a a expiry Close
      $ deposit underlying ratio b b expiry Close
      $ pay a b currency strike
      $ pay b a underlying ratio
        Close
    transfer Put a b = transfer Call b a

-- Option Strategies

-- |"A straddle is a neutral options strategy that involves simultaneously buying both
-- a put option and a call option for the underlying security with the same strike price
-- and the same expiration date." -- investopedia
straddle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Ratio
  -> Value    -- ^ Strike
  -> Timeout  -- ^ Maturity
  -> Contract -- ^ Straddle Contract
straddle buyer seller currency underlying ratio strike maturity =
  let c = european Call buyer seller currency underlying ratio strike maturity
      p = european Put  buyer seller currency underlying ratio strike maturity
   in c `both` p

-- |"A strangle is an options strategy in which the investor holds a position in both
-- a call and a put option with different strike prices, but with the same expiration
-- date and underlying asset." -- investopedia
strangle ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Ratio
  -> Value    -- ^ Lower Strike
  -> Value    -- ^ Upper Strike
  -> Timeout  -- ^ Maturity
  -> Contract -- ^ Straddle Contract
strangle buyer seller currency underlying ratio strike1 strike2 maturity =
  let c = european Call buyer seller currency underlying ratio strike1 maturity
      p = european Put  buyer seller currency underlying ratio strike2 maturity
   in c `both` p

-- |"A bull call spread is an options trading strategy designed to benefit from a stock's
-- limited increase in price. The strategy uses two call options to create a range consisting
-- of a lower strike price and an upper strike price." -- investopedia
callSpread ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Token    -- ^ Currency
  -> Token    -- ^ Underlying
  -> Value    -- ^ Strike price (in currency) for the long position
  -> Value    -- ^ Strike price (in currency) for the short position
  -> Value    -- ^ Amount of underlying tokens per contract
  -> Timeout  -- ^ Maturity
  -> Contract -- ^ Call Spread Contract
callSpread buyer seller currency underlying strike1 strike2 ratio maturity =
  let s = european Call buyer seller currency underlying ratio strike1 maturity
      l = european Call seller buyer currency underlying ratio strike2 maturity
   in s `both` l

-- Helper functions
-- TODO: move to Language.Marlowe.Extended

both :: Contract -> Contract -> Contract
both = undefined

pay :: Party -> Party -> Token -> Value -> Contract -> Contract
pay a b = Pay a (Party b)

deposit :: Token -> Value -> Party -> Party -> Timeout -> Contract -> Contract -> Contract
deposit token amount by toAccount timeout timeoutContinuation continuation =
  When
    [Case (Deposit toAccount by token amount) continuation]
    timeout
    timeoutContinuation
