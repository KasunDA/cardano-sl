{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Base of GodTossing SSC.

module Pos.Ssc.GodTossing.Types.Base
       (
         -- * Types
         Commitment (..)
       , CommitmentSignature
       , SignedCommitment
       , CommitmentsMap
       , Opening (..)
       , OpeningsMap
       , SharesMap
       , VssCertificate (..)
       , mkVssCertificate
       , VssCertificatesMap
       , PKSet
       ) where


import           Data.Binary         (Binary)
import           Data.MessagePack    (MessagePack)
import           Data.SafeCopy       (base, deriveSafeCopySimple)
import           Data.Text.Buildable (Buildable (..))
import           Universum

import           Pos.Crypto          (LEncShare, LSecret, LSecretProof,
                                      LSecretSharingExtra, LShare, LVssPublicKey,
                                      PublicKey, SecretKey, Signature, sign, toPublic)
import           Pos.Types.Types     (Address (..), EpochIndex)

----------------------------------------------------------------------------
-- Types, instances
----------------------------------------------------------------------------
type PKSet = HashSet Address

-- | Commitment is a message generated during the first stage of
-- MPC. It contains encrypted shares and proof of secret.
data Commitment = Commitment
    { commExtra  :: !LSecretSharingExtra
    , commProof  :: !LSecretProof
    , commShares :: !(HashMap LVssPublicKey LEncShare)
    } deriving (Show, Eq, Generic)

instance Binary Commitment

-- | Signature which ensures that commitment was generated by node
-- with given public key for given epoch.
type CommitmentSignature = Signature (EpochIndex, Commitment)

type SignedCommitment = (Commitment, CommitmentSignature)

type CommitmentsMap = HashMap Address (Commitment, CommitmentSignature)

-- | Opening reveals secret.
newtype Opening = Opening
    { getOpening :: LSecret
    } deriving (Show, Eq, Generic, Binary, Buildable)

type OpeningsMap = HashMap Address Opening

-- | Each node generates a 'SharedSeed', breaks it into 'Share's, and sends
-- those encrypted shares to other nodes. In a 'SharesMap', for each node we
-- collect shares which said node has received and decrypted.
--
-- Specifically, if node identified by 'Address' X has received a share
-- from node identified by key Y, this share will be at @sharesMap ! X ! Y@.
type SharesMap = HashMap Address (HashMap Address LShare)

-- | VssCertificate allows VssPublicKey to participate in MPC.
-- Each stakeholder should create a Vss keypair, sign public key with signing
-- key and send it into blockchain.
--
-- A public key of node is included in certificate in order to
-- enable validation of it using only node's P2PKH address.
--
-- Other nodes accept this certificate if it is valid and if node really
-- has some stake.
data VssCertificate = VssCertificate
    { vcVssKey     :: !LVssPublicKey
    , vcSignature  :: !(Signature LVssPublicKey)
    , vcSigningKey :: !PublicKey
    } deriving (Show, Eq, Generic)

instance Binary VssCertificate

mkVssCertificate :: SecretKey -> LVssPublicKey -> VssCertificate
mkVssCertificate sk vk = VssCertificate vk (sign sk vk) $ toPublic sk

-- | VssCertificatesMap contains all valid certificates collected
-- during some period of time.
type VssCertificatesMap = HashMap Address VssCertificate

deriveSafeCopySimple 0 'base ''VssCertificate
deriveSafeCopySimple 0 'base ''Opening
deriveSafeCopySimple 0 'base ''Commitment
