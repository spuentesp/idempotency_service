# Idempotency Service

An OTP (Open Telecom Platform) application designed to normalize and handle non-idempotent data by applying configurable rules to ensure idempotency. This service reads a set of user-defined rules from a configuration file and uses pattern matching to generate consistent idempotency keys.

---

## Features
- Configurable idempotency rules defined in configuration files.
- Pattern matching for flexible normalization.
- OTP-compliant structure for scalability and maintainability.

---

## Installation

### Prerequisites
- Erlang/OTP 27 or later
- rebar3 (version 3.24.0 or later)
- OpenSSL (version 3.0 or later)

### Clone the Repository
```sh
git clone <repository_url>
cd idempotency_service
```

### Environment Configuration
If you encounter issues with SSL certificates during dependency installation, ensure the following:

```sh
export SSL_CERT_FILE=/opt/homebrew/Cellar/openssl@3/3.4.0/etc/cacert.pem
```

### Build the Project
Compile the application using rebar3:
```sh
rebar3 compile
```

## Usage
### Loading Configuration
Place your idempotency rules in a configuration file, for example, idempotency_rules.config, located in the root directory. An example of the configuration structure:
```sh
[
  {idempotency_service, [
    {rules, [
      {default, #{ignore_fields => ["timestamp"],
                  key_fields => all,
                  normalization_rules => [
                    #{field => "amount", type => float},
                    #{field => "user_id", type => string}
                  ],
                  default_action => ignore}},
      {payment, #{ignore_fields => ["timestamp", "request_id"],
                  key_fields => ["user_id", "amount"],
                  normalization_rules => [
                    #{field => "amount", type => float},
                    #{field => "user_id", type => string}
                  ],
                  default_action => reject}},
      {user_event, #{ignore_fields => ["timestamp", "event_id"],
                    key_fields => ["user_id", "event_type"],
                    normalization_rules => [
                      #{field => "user_id", type => string},
                      #{field => "event_type", type => string}
                    ],
                    default_action => ignore}}
    ]},
    {error_handling, #{invalid_data => log_and_continue,
                      missing_fields => reject}}
  ]}
].
```

### Running in Shell
Launch the interactive shell:
```sh
rebar3 shell
```

Load the configuration file:

```sh
config_loader:load_config("server.config").
```
or
```sh
config_loader:load_config("test/server_test.config").
```

## Project Structure

```sh
idempotency_service/
├── src/                       # Source files
│   ├── config_loader.erl      # Module for loading and parsing configuration
│   ├── idempotency_service.erl # Main application module
│   ├── normalizer.erl         # Module for normalizing requests
├── test/                      # Test files
│   ├── config_loader_test.erl # Unit tests for config_loader
│   ├── idempotency_test.erl   # Unit tests for idempotency logic
│   ├── normalizer_test.erl    # Unit tests for normalizer
├── [idempotency_rules.config](http://_vscodecontentref_/0)   # Example rules (user-defined)
├── _build/                    # Build artifacts (generated)
├── [rebar.config](http://_vscodecontentref_/1)               # Rebar3 configuration
└── [README.md](http://_vscodecontentref_/2)                  # Project documentation
```


## Server Workflow
Server Start: The server starts and loads the server.config file to get the path to the idempotency_rules.config file.
Load Configurations: The server loads the idempotency rules from the idempotency_rules.config file.
Supervisor Initialization: The server initializes a supervisor that spawns processes for normalization and idempotency checks.
Normalization and Idempotency Check: The processes use the loaded rules to normalize incoming requests and check for idempotency.
Default Actions:
Ignore: If a message is found to have been sent previously, it is ignored.
Reject: If a message is found to have been sent previously, an error message is returned.
Diagram
```sh
@startuml
!define RECTANGLE class

RECTANGLE Server {
  + start()
  + load_config(file)
  + load_rules(file)
}

RECTANGLE Supervisor {
  + start()
  + spawn_processes()
}

RECTANGLE Normalizer {
  + normalize(request)
}

RECTANGLE IdempotencyChecker {
  + check_idempotency(request)
}

Server -down-> Supervisor : initializes
Supervisor -down-> Normalizer : spawns
Supervisor -down-> IdempotencyChecker : spawns

Server : loads [server.config](http://_vscodecontentref_/3)
Server : loads [idempotency_rules.config](http://_vscodecontentref_/4)
Normalizer : uses rules for normalization
IdempotencyChecker : uses rules for idempotency check

@enduml
```

## Testing
### Running Tests
To run the tests, use the following command:
```sh
rebar3 eunit
```

## Roadmap
### Done
#### Module: config_loader

Load rules from a configuration file.
Parse configuration into maps for pattern matching.

#### Basic Structure:

src/ folder with OTP-compliant modules.
Rebar3 build system initialized.
SSL Configuration Fixes:

Instructions for setting SSL_CERT_FILE to resolve dependency installation issues.

Develop Core Normalization Logic

Apply rules to incoming data to generate idempotency keys.
Error Handling

### To Do


Graceful handling for invalid configuration or missing keys.
Testing Suite:

Add unit tests for config_loader and normalization logic.
CLI Support:

Allow users to test configurations and transformations via command line.
Docker Integration:

Create a Dockerfile for containerized deployment.
Documentation:

Expand this README with examples and edge cases.
CI/CD Pipeline:

Automate tests and builds with GitHub Actions or similar.

##Contributing
Contributions are welcome! Please fork the repository and submit a pull request.

## License
This project is licensed under the MIT License. See the LICENSE file for details.

