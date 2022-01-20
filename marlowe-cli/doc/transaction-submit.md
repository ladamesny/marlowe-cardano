# Marlowe CLI: Submitting Transactions

The `marlowe-cli transaction submit` command submits a transaction body to the blockchain.


## Options

    $ marlowe-cli transaction submit
    
    Usage: marlowe-cli transaction submit [--testnet-magic INTEGER]
                                          --socket-path SOCKET_FILE
                                          --tx-body-file BODY_FILE
                                          [--required-signer SIGNING_FILE]
                                          [--timeout SECONDS]
      Submit a transaction body.
    
    Available options:
      --testnet-magic INTEGER         Network magic, or omit for mainnet.
      --socket-path SOCKET_FILE       Location of the cardano-node socket file.
      --tx-body-file BODY_FILE        File containing the transaction body.
      --required-signer SIGNING_FILE  File containing a required signing key.
      --timeout SECONDS               Also submit the transaction, and wait for confirmation.
      -h,--help                       Show this help text
