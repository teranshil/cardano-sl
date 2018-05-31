-- | Common core types essential for multiple components.
module Pos.Core.Common.Types
       (
       -- * Address and StakeholderId
         AddressHash
       , AddrSpendingData (..)
       , AddrType (..)
       , Address' (..)
       , AddrAttributes (..)
       , AddrStakeDistribution (..)
       , MultiKeyDistrError (..)
       , mkMultiKeyDistr
       , Address (..)

       -- * Stakeholders
       , StakeholderId
       , StakesMap
       , StakesList

       -- * ChainDifficulty
       , ChainDifficulty (..)

       , SharedSeed (..)
       , SlotLeaders
       , slotLeadersF

       -- * Coin
       , Coin (..)
       , CoinPortion (..)
       , mkCoin
       , checkCoin
       , coinF
       , unsafeGetCoin
       , maxCoinVal

       -- * Scripting
       , Script(..)
       , Script_v0
       , ScriptVersion

       -- * Newtypes
       -- ** for amounts
       , BlockCount(..)

       , addressHash

       , module Pos.Core.Common.CoinPortion
       ) where

import           Pos.Core.Common.AddrAttributes
import           Pos.Core.Common.Address
import           Pos.Core.Common.AddressHash
import           Pos.Core.Common.AddrSpendingData
import           Pos.Core.Common.AddrStakeDistribution
import           Pos.Core.Common.BlockCount
import           Pos.Core.Common.ChainDifficulty
import           Pos.Core.Common.Coin
import           Pos.Core.Common.CoinPortion
import           Pos.Core.Common.Script
import           Pos.Core.Common.SharedSeed
import           Pos.Core.Common.SlotLeaders
import           Pos.Core.Common.StakeholderId
import           Pos.Core.Common.Stakes
