---
tags: tooling, deku
---
# Deku as a plugin of Taqueria

## Deku workflow 
- Tickets:
Deku works with bytes ticket's. So if your metadata is some other type besides bytes, you'd have to create a "proxy" ticket using PACK to convert your arbitrary Michelson type into bytes.
- Deposit: 
You deposit tickets with the deposit endpoint. You can see it on [better-call.dev](https://better-call.dev/).
    + Once you deposit the tickets to Deku, they're locked inside the **Deku contract's vault**. Deku uses `tezos-client` to deposit (more in the example of deposit dummy ticket).
    + Once finality on that block is reached, the Deku nodes will mint tickets with identical contents on the Deku chain. These can be transferred/sent to smart contracts running on Deku/etc.
- Withdraw:
The withdraw process is a bit more complicated.
    + Users can withdraw from Deku using the `deku-cli withdraw` command, which burns the tickets on Deku's side.
    + Once the withdraw is finalized in a block, you can ask a Deku node for a withdraw proof.
    + After the next state root update from the Deku nodes' to Tezos, you can use the withdraw proof to unlock the tickets that were burned from Deku's vault on Tezos.
    + Then, the contract specified in the withdraw can collect the tickets by calling the withdraw endpoint of the Deku contract.

## Deku configurations
- Currently, Deku has 3 nodes: one is a producer, and two nodes are validators. In localhost mode, the node information is stored in the folder `data`. 

Deku has an [alphanet chain](https://www.marigold.dev/deku). The information of 3 nodes on alphanet is:

```
https://peer-0.deku.marigold.dev
https://peer-1.deku.marigold.dev
https://peer-2.deku.marigold.dev
```

- Deku uses this secret key to sign Tezos operations:
```
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
```
- Deku bootstrapper:
Deku uses this secret key to bootstrap the chain:
```
DEKU_BOOTSTRAPPER_SECRET="edsk3CRD8RNhoTPV74p8t9pEvW3zEH2ZZzVD8zns2dyZyY49nbjoiG"
```
or if you want to create a new one, you can run the command:

```
deku-bootstrapper setup-identity
```

- Deku uses the `tezos-client` from Flextesa run by docker:
The information of docker setting can be found at `docker-compose.yml`

```
flextesa:
    container_name: deku_flextesa
    restart: always
    image: oxheadalpha/flextesa:20220510
    command: jakartabox start
    environment:
      - block_time=4
    ports:
      - "20000:20000"
    expose:
      - 20000/tcp
```

The `RPC_NODE` setting in Deku:

```
if [ $mode = "docker" ]; then
  RPC_NODE=http://flextesa:20000
else
  RPC_NODE=http://localhost:20000
fi
```

Deku uses `tezos-client` to deploy contracts: discovery and consensus written in mligo

- Deku smart contracts:
    - Deku has two main contracts: discovery and consensus and one dummy contract (for testing: `deku-sandbox`)
        - [consensus.mligo](https://github.com/marigold-dev/deku/blob/main/src/tezos_interop/consensus.mligo): contain 3 entrypoints: `update_root_hash`, `deposit` and `withdraw`, where the storare contains the `root_hash` and the `vault`.
        - [discovery.mligo](https://github.com/marigold-dev/deku/blob/main/src/tezos_interop/discovery.mligo): contain one entrypoint: `uri_update` and the storage is a big_map of a pair `(key_hash, (nonce, uri))`.
        - [dummy_ticket.mligo](https://github.com/marigold-dev/deku/blob/main/dummy_ticket.mligo): This smart contract is a testing contract. It has 3 entrypoint: `mint_to_deku`, `withdraw_from_deku` and `burn_callback`.
    - Deploy smart contracts:
        + Using Ligo to convert `mligo` to `tz`
        + Then use `tezos-client --endpoint $RPC_NODE originate contract ...` to originate the contracts. Where the sender of this origination is the Deku wallet.
    - The smart contracts address originated on Deku alphanet:
        ```
        - Bridge: KT1JXWdpCDSep84PyqbJYZmuzNWuP8F6FCjo
        - Discovery: KT1GqDjZkS4q73ynADAQ61dwECc6RSt2c657
        ```
- Tezos accounts
Deku uses two accounts: alice and bob from Flextesa
- Deku has these CLIs
This info can be found in the folder `bin` in the source code
    ```
    deku-node
    deku-cli
    deku-bootstrapper
    deku-sandbox
    ```
    - `deku-node`: the deku nodes store the secret of the node for instance: bootstrapper secret key, identity, tezos information (consesus contract address, discovery contract address, etc.), etc. You can run in localhost, or the node on the alphanet:
        ```
        docker compose up -d
        ./sandbox.sh setup
        ```
    And the folder `data/i` is generated with all the information of the Deku     nodes.
    - `deku-node` supports:
        - `deku-node add-trusted-validator`
        - `deku-node remove-trusted-validator`
        - `deku-node produce-block`
        - `deku-node sign-block`
        - `deku-node self`
        - `deku-node setup-identity`: create a validator identity, this setup identity which uri the node is running on, for instance: `deku-node setup-identity <node_folder> --uri "http://localhost:4440"`
        - `deku-node setup-tezos`: setup tezos identity. The information of this setup on localhost is stored in `data/tezos.json`
        - `deku-node start`
    - `deku-cli` you can get the `deku-cli` by cloning the [Deku-Canonical (Deku-C)](https://github.com/marigold-dev/deku) and then compile this source code, this cli supports:
        - `deku-cli create-transaction`
        - `deku-cli create-wallet`
        - `deku-cli get-balance`
        - `deku-cli originate-contract`
        - `deku-cli self`
        - `deku-cli withdraw`
        - `deku-cli withdraw-proof`
    - `deku-bootstrapper` supports:
        - `deku-bootstrapper bootstrap`
        - `deku-bootstrapper derive-key`
        - `deku-bootstrapper setup-identity`
    - `deku-sandbox` supports:
        - `deku-sandbox setup`
        - `deku-sandbox start`
        - `deku-sandbox tear-down`
        - `deku-sandbox deploy-dummy-ticket`
        - `deku-sandbox deposit-dummy-ticket`
        - `deku-sandbox deposit-withdraw-test`
        - `deku-sandbox smoke-test`
        
## Deku as a plugin on Taqueria

First idea: 

What Deku needs:
- Ligo plugin: compile, createContract of ligo
- Flextesa plugin
- Smart contracts
    - template files contain the discovery, consensus and dummy smart contracts of Deku
- index: contains tasks for deku plugin


## References
- Deku-C: https://github.com/marigold-dev/deku
    - Sandbox configure: https://github.com/marigold-dev/deku/blob/main/sandbox.sh
- Deku alphnet: https://www.marigold.dev/deku
- TezPortal: https://hackmd.io/WdxZjSWKRoey7s8wMQlPiQ?view
- Transaction on Deku: https://hackmd.io/pnkkM0w6Q7ONYxFYPGIuXg?view
