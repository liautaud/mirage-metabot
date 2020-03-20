# 🤖 mirage-metabot

_One bot to rule them all, one bot to find them,  
One bot to bring them all  and in the darkness bind them._

## About

`metabot` is a programmable Slack bot: it lets you customize its behavior interactively by writing [OCaml](https://ocaml.org/) code. 

Once you install `metabot` in your Slack workspace, you can start chatting with it by writing OCaml expressions–just like in your favorite interpreter. It will evaluate each of them, and reply with the value. Since `metabot` comes with a [standard library](#) which covers most aspects of the Slack API–including sending messages to channels, triggering notifications, or listening for incoming messages–you can effectively **use it to write new Slack bots**.

_Note: For the time being, only [a limited subset of OCaml](https://github.com/flupe/fouine) is supported by `metabot`._

## Getting started

`metabot` is written as a [MirageOS](https://mirage.io/) unikernel, so you can run it directly under a [Xen](http://xen.org/) or [KVM](http://www.linux-kvm.org/page/Main_Page) hypervisor. This allows `metabot` to run more efficiently, securely and with finer control than with a full conventional software stack.

That said, the simplest way to start running `metabot` is to compile it directly as a native Unix or Mac OS binary.

#### 1. Compiling and running the unikernel.

1. Install opam for your operating system using [these instructions](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system).
2. Initialize opam for your shell using `opam init`.
3. Create a new opam switch using `opam switch metabot`.
4. Install the mirage tool using `opam install mirage`.
5. Configure `metabot` for your operating system:
    - On Unix systems, `cd src && mirage configure -t unix --dhcp --net=direct`.
    - On Mac OS systems, `cd src && mirage configure -t macos --dhcp --net=direct`.
6. Install the dependencies using `make depend`.
7. Compile using `make`.
8. Finally, run using `./metabot`.

    You need to pass the following configuration options, either as command-line arguments or environment variables:
    ```
    REQUIRED CONFIGURATION:
        --bot_token=VAL or METABOT_BOT_TOKEN (required)
            Bot user access token for the Slack API. This key is required.

        --client_id=VAL or METABOT_CLIENT_ID (required)
            Client ID for the Slack API. This key is required.

        --client_secret=VAL or METABOT_CLIENT_SECRET (required)
            Client secret for the Slack API. This key is required.

        --signing_secret=VAL or METABOT_SIGNING_SECRET (required)
            Signing secret for the Slack API. This key is required.

    OPTIONAL CONFIGURATION:
        --port=VAL or METABOT_PORT (absent=8080)
            TCP port used by the HTTP server.

        -l LEVEL, --logs=LEVEL or MIRAGE_LOGS (absent=*:debug)
            Be more or less verbose. LEVEL must be of the form *:info,foo:debug
            means that that the log threshold is set to info for every log
            sources but the foo which is set to debug.
    ```


#### 2. Registering the application in your Slack workspace.