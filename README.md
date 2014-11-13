## Parse IRC output ##

From: `:irc.server.org 332 Innocence #lobby :Let's all have great fun! :D`
To: 
<pre>(
    'irc.server.org', -- server
    '#lobby', -- channel
    Nothing, -- recipient
    Nothing, -- user
    '332', -- type
    'Let us all have great fun! :D' -- msg
)</pre>
    
This will indicate that a topic has been found for a channel. An example of 
a user action would be:

From: `:Tehnix!Tehnix@ghost-EC31B3C1.rdns.scalabledns.com NICK :BlaBliBlu`
To: 
<pre>(
    Nothing, -- server
    Nothing, -- channel
    Nothing, -- recipient
    ('Tehnix', 'Tehnix', 'ghost-EC31B3C1.rdns.scalabledns.com'), -- user
    'NICK', -- type
    'BlaBliBlu' -- msg
)</pre>

Here, the user changed his nickname from 'Tehnix' to 'BlaBliBlu'.