1. Run contract in README.md

2. Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `GameContract` one.

#### Playing the guessing game over the API

The game has two players (wallets). One will initialise the contract and lock a value inside. Another
wallet will then make guesses. Supposing they guess correctly, they'll receive the funds that were
locked; otherwise, they won't!

1. Start the instances:

```
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "CurrencyContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "CurrencyContract", "caWallet":{"getWallet": 2}}' \
  http://localhost:8080/api/new/contract/activate | jq
```

From these two queries you will get back two contract instance IDs. These will be needed
in the subsequent steps for running actions against. We can optionally take a look at the state
of the contract with the `status` API:

2. Get the status

```
export INSTANCE_ID=...
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq
```

This has a lot of information; and in particular we can see what endpoints are still available
to call.

3. Start the game by locking some value inside

Get forge curremcy parameters

```
cabal repl

import Contracts.Currency
import Ledger.Value   
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as B
args = SimpleMPS { Contracts.Currency.tokenName = TokenName $ B.pack "testToken", amount = 10 }
BSL.putStrLn $ encode args

{"tokenName":{"unTokenName":"testToken"},"amount":10}
```

4. Lock some value (Wallet 1)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"tokenName":{"unTokenName":"testToken"},"amount":10}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/createNativeToken
```

