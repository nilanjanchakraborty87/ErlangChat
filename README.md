# ErlangChat

Yaychat is a multiuser chat application written using erlang otp applications.

# Installations
* **Erlang** - People using Windows can get the binaries from https://www.erlang.org/downloads, and most linux or unix-like distributions can get those from package managers. OSX is fine with Homebrew.

* **Rebar3**- To install rebar3 follow the below steps. Please clone Tag 3.4.0
    ``` 
    $ git clone https://github.com/erlang/rebar3.git
    $ cd rebar3
    $ ./bootstrap
    $ PATH=/usr/local/rebar3:$PATH. 
    $ export PATH
    ``` 
Now clone ErlangChat as follows -

  ```
  $ git clone https://github.com/nilanjanchakraborty87/ErlangChat.git
  ```
  
### Start Chat Server
To start the chat server run the below commands -

  ```
  $ cd yaychat
  $ rebar3 shell --apps yaychat
  ```
  
### Start Chat Client
To start the chat client run below commands -

  ```
  $ cd yay-client
  $ rebar3 shell --apps yay_client
  ```
  
Enjoy Chatting!

