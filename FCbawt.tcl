# --------------------------------------------------------------------
# Multi.tcl $scriptversion (c)2002 Key2Peace Original Coder.         #
# 								     #
# 2019 - New Mod by Rveyn @ irc.rizon.fun                            #
#                                                                    #
#  mod.FCbawt $fcbversion (c)2019 Rveyn                              #
# --------------------------------------------------------------------
# New in mod.FCbawt v-1.0:                                           #
#                                                                    #
# Adding a help commands (help, ghelp, lhelp, uhelp                  #
# Adding +/- vonjoin setting. (auto join on voice)                   #
# Adding $contactchan for informe the help channel about the box     #
# Adding TODO List                                                   #
#                                                                    #
#                                                                    #
# -----------------------------------------------------------------  #


#########################
## Set global variables #
#########################

#load

source mod.FCbawt/required/httpd.tcl
source mod.FCbawt/required/json.tcl

#Require 

package require http
package require tdom

#Bot reponds to nick and this shortnick (botnick help / bn help)
set shortnick "sis"

# if enable +debug, it's the debug channel for stuff
set home "#rveyn.staff"

#Channel to contact owner
set contactchan "#soutien"

#Botname name or your project name, used in announce etc..
set service "weedos"

#Identify bot owner $nickowner <$mailowner> 
set nickowner "Rveyn"
set mailowner "support@supah.net"

# Channel Service nick (eg. X on undernet, Q on quakenet, ...)
set cservice "W"


## ----------------------------------------------------------------
## Do me and yourself a favour and stay the fuck outta there 
## ----------------------------------------------------------------

## ----------------------------------------------------------------
## Specific Settings - Don't remove script right please.
## ----------------------------------------------------------------


set scriptversion "v3.6m3"
set fcbversion "v-1.0"
set ctcp-version "eggdrop[lindex $version 0] :: Running Multi $scriptversion (c)2002 Key2Peace :: mod.FCbawt $fcbversion (c)2019 Rveyn"
set ctcp-userinfo "Bot Admin: $nickowner <$mailowner>"
set keep-nick 1
set use-ison 1
set ctcp-mode 0
set strict-host 0

## ----------------------------------------------------------------
## --- INITIALIZE CHANNELFLAGS                                  ---
## ----------------------------------------------------------------
setudef flag topiclock
setudef flag chanvoice
setudef flag chanop
setudef flag norejoin
setudef flag nocolour
setudef flag nocaps
setudef flag nonotice
setudef flag badchan
setudef flag badword
setudef flag norepeat
setudef flag membersonly
setudef flag chandb
setudef flag noopsidle
setudef flag novoiceidle
setudef flag nonickchange
setudef flag limiter
setudef flag autotopic
setudef flag suspend
setudef flag debug
setudef int chanlimit
setudef flag ip

## ----------------------------------------------------------------
## --- LOGIN/LOGOUT					        ---
## ----------------------------------------------------------------
bind msg - login msg_login
bind msg - logout msg_logout
proc msg_login {nick uhost hand rest} {
  global botnick
  set pw [lindex $rest 0]
  set op [lindex $rest 1]
  if {$pw == ""} {
  putnotc $nick "Usage: /msg $botnick login <password> \[recover\]"
  return 0 }
  if {[matchattr $hand Q]} {
  if {[string tolower $op] == "recover"} {
  if {[passwdok $hand $pw]} {
  setuser $hand XTRA SECNICK $nick
  setuser $hand XTRA SECHOST $uhost
  putnotc $nick "New Identity confirmed. Recover Successful" }
  if {![passwdok $hand $pw]} {
  putnotc $nick "Wrong password. Recover failed !"
  return 0 }
  return 0 }
  putnotc $nick "You are already Authenticated."
  putnotc $nick "Nick: [getuser $hand XTRA SECNICK]"
  putnotc $nick "Host: [getuser $hand XTRA SECHOST]"
  putnotc $nick "Try to login with /msg $botnick login <pass> recover"
  return 0 }
  if {[passwdok $hand $pw] == 1} {
  chattr $hand +Q
  putnotc $nick "Authentication successful as $hand!"
  setuser $hand XTRA SECNICK $nick
  setuser $hand XTRA SECHOST $uhost }
  if {[passwdok $hand $pw] == 0} {
  putnotc $nick "Authentication failed!" }}

proc msg_logout {nick uhost hand rest} {
  if {[getuser $hand XTRA SECNICK] == $nick} {
  chattr $hand -Q
  setuser $hand XTRA SECNICK $nick
  setuser $hand XTRA SECHOST $nick
  putnotc $nick "DeAuthentication successful!" }}


## ----------------------------------------------------------------
## --- ADDHOST						        ---
## ----------------------------------------------------------------
unbind msg - addhost *msg:addhost
bind msg - addhost securedeny
proc securedeny {nick host hand args} {
  global contactchan
  putnotc $nick "Please ask in $contactchan to get help." }

## ----------------------------------------------------------------
## --- IDENT						        ---
## ----------------------------------------------------------------
unbind msg - ident *msg:ident
bind msg - ident secureident
proc secureident {nick host hand args} {
  global home botnick contactchan
  set args [lindex $args 0]
  set pass [lindex $args 0]
  set idnt [lindex $args 1]
  set mask [bmaskhost $host]
  if {$args == ""} {
  putnotc $nick "Usage: /msg $botnick ident <pass> <handle>"
  return 0}
  if {$idnt == ""} {
  putnotc $nick "Your handle is required for this. If you need assistance ask in $contactchan"
  return 0}
  if {[passwdok $idnt $pass]} {
  putnotc $nick "Hello $idnt. Nice to see you back. Adding $mask to your hostlist"
  setuser $idnt HOSTS $mask
  setuser $idnt XTRA SECNICK $nick
  setuser $idnt XTRA SECHOST $host
  homewall "$nick \($host\) succeeded IDENT as $idnt"
  putlog "\($nick!$host\) !$idnt! IDENT as $idnt"
  return 0 }
  if {[passwdok $pass $idnt]} {
  putnotc $nick "Usage: /msg $botnick ident <pass> <handle>"
  return 0 }
  putnotc $nick "Please ask in $contactchan to get help."
  putlog "\($nick!$host\) !$idnt! FAILED IDENT"
  homewall "$nick \($host\) failed IDENT as $idnt" }

## ----------------------------------------------------------------
## --- (UN)SUSPEND                                              ---
## ----------------------------------------------------------------
bind msg m suspend msg_suspendsys
bind msg m unsuspend msg_suspendsys

proc msg_suspendsys {nick host hand args} {
  global home botnick lastbind
  set args [lindex $args 0]
  set chan [lindex $args 0]
  set reas [lrange $args 1 end]
  set stat [string tolower $lastbind]
  if {![checksec $nick $host $hand]} { return 0 }
  if { $args == "" } {
  putnotc $nick "Usage: /msg $botnick $stat <chan> \[reason\]"
  return 0 }
  if {![validchan $chan]} {
  putnotc $nick "Sorry but I don't monitor $chan"
  return 0 }
  switch $stat {
  suspend { if {[ischanset $chan suspend]} {
            putnotc $nick "$chan is already suspended"
	    return 0 }
            if { $reas == "" } {
  	    putnotc $nick "Sorry, but you need to specify a reason"
	    return 0 }
	    putserv "PART $chan :Channel has been suspended by $hand \($reas\)"
	    channel set $chan +suspend +inactive
	    return 1 }
  unsuspend { if {![ischanset $chan suspend]} {
              putnotc $nick "$chan is not suspended"
	      return 0 }
	      channel set $chan -suspend -inactive
	      return 1 }
 }
}

## ----------------------------------------------------------------
## --- SUBROUTINES					        ---
## ----------------------------------------------------------------
bind filt - "\001ACTION *\001" filt:dcc_action
bind filt - "/me *" filt:telnet_action

if {![file exists "{[string tolower $network]}database.info"]} {
  set fd [open "{[string tolower $network]}database.info" w]
  puts $fd "."
  close $fd }

proc isupper {letter} {
  set caps {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z}
  if {[lsearch -exact $caps $letter] > -1} {  return 1 } { return 0 }}

proc homewall {msg} {
  global home botnick service
  foreach user [chanlist $home] {
  if {[matchattr [nick2hand $user $home] m]} {
  putnotc $user "\[$service\] $msg" }}}

proc ischanset {chan setting} {
  set setting "+[string tolower $setting]"
  foreach item [string tolower [channel info $chan]] {
  if {$item == $setting} { return 1 }}
  return 0 }

proc checksec {nick host hand} {
  global botnick
  if {![matchattr $hand +Q]} {
  putnotc $nick "You are not authenticated."
  putnotc $nick "Please login with /msg $botnick login <password>."
  return 0}
  if {[getuser $hand XTRA SECNICK] != $nick} {
  putnotc $nick "Sorry. But I think I missed one of your nickchanges"
  return 0}
  if {[getuser $hand XTRA SECHOST] != $host} {
  putnotc $nick "Sorry. But you don't have the correct host right now."
  return 0}
  return 1}

proc filt:dcc_action {idx text} {
  return ".me [string trim [join [lrange [split $text] 1 end]] \001]" }

proc filt:telnet_action {idx text} {
  return ".me [join [lrange [split $text] 1 end]]" }

proc time {} {
  strftime "%H:%M" }

proc date {} {
  strftime "%d %b %Y" }

proc getstatus {nick chan} {
  set user [nick2hand $nick $chan]
  if {![matchattr $user Q] || [getuser $user XTRA SECHOST] != [getchanhost $nick $chan] ||  \
      [string tolower [getuser $user XTRA SECNICK]] != [string tolower $nick] } {
      return "Not logged in" }
  if {[matchattr $user B]} { return "The one that was, the one that is, the one that will be" }
  if {[matchattr $user O]} { return "IRCop" }
  if {[matchattr $user b]} { return "Service Bot" }
  if {[matchattr $user n]} { return "Global Owner" }
  if {[matchattr $user m]} { return "Global Master" }
  if {[matchattr $user a]} { return "Service Bot" }
  if {[matchattr $user o]} { return "Global Operator" }
  if {[matchattr $user &n $chan]} { return "Channelowner of $chan" }
  if {[matchattr $user &m $chan]} { return "Channelmaster of $chan" }
  if {[matchattr $user &a $chan]} { return "Channelbot of $chan" }
  if {[matchattr $user &o $chan]} { return "Channelop of $chan" }
  if {[matchattr $user &v $chan]} { return "Channelvoice of $chan" }
  if {[matchattr $user &f $chan]} { return "Regular of $chan" }
  if {[matchattr $user H $chan]} { return "Bothub" }
  return "Unknown" }

proc greetparse {nick host handle chan text} {
  global botnick server
  regsub -all %N $text $nick text
  regsub -all %H $text $host text
  regsub -all %S $text $server text
  regsub -all %C $text $chan text
  regsub -all %L $text [getstatus $nick $chan] text
  regsub -all %U $text $handle text
  regsub -all %B $text $botnick text
return $text }

proc putmsg {target message} {
  global support
  if {![info exists support(cprivmsg)] || [validchan $target]} {
  putserv "PRIVMSG $target :$message"
  return 0
  } else {
  foreach chan [channels] {
  if {[onchan $target $chan] && ([botisop $chan] || [botisvoice $chan])} {
  putserv "CPRIVMSG $target $chan :$message"
  return 0 }}
  putserv "PRIVMSG $target :$message" }}
 
proc putnotc {target message} {
  global support
  if {![info exists support(cnotice)] || [validchan $target]} {
  putserv "NOTICE $target :$message"
  return 0
  } else {
  foreach chan [channels] {
  if {[onchan $target $chan] && ([botisop $chan] || [botisvoice $chan])} {
  putserv "CNOTICE $target $chan :$message"
  return 0 }}
  putserv "NOTICE $target :$message" }}

proc putdebug {message} {
  global home
  if {![ischanset $home debug] || ![botonchan $home] } { return 0 }
  putserv "PRIVMSG $home :$message" }
  
proc charfilter {data} {
  regsub -all -- \\\\ $data \\\\\\\\ data
  regsub -all -- \\\[ $data \\\\\[ data 
  regsub -all -- \\\] $data \\\\\] data 
  regsub -all -- \\\} $data \\\\\} data
  regsub -all -- \\\{ $data \\\\\{ data
  regsub -all -- \\\" $data \\\\\" data 
  return $data }
    
proc bmaskhost {uhost} {
  set uhost [lindex [split $uhost !] [expr [llength [split $uhost !]] -1]]
  set temp [string range [lindex [split $uhost @] 0] [expr [string length [lindex [split $uhost @] 0]] - 8] e]
  regsub -all ~ $temp "" temp
  if {[maskhost *!*@[lindex [split $uhost @] 1]] == "*!*@*.undernet.org"} {
    set host [lindex [split $uhost @] 1]
    set temp ""
  } else {
    set host [lindex [split [maskhost *!*@[lindex [split $uhost @] 1]] @] 1]
  }
  return "*!*$temp@$host" }

proc mchnick {nick handle oldn newn} {
  if {![validuser $oldn]} {
    putnotc $nick "I can't find anyone matching $oldn in the bot's userlist"
    return 1 }
  if {[matchattr $oldn n]} {
    putnotc $nick "You can not change the handle of a botowner!"
    return 1 }
  if {![matchattr $handle n] && [matchattr $oldn m]} {
    putnotc $nick "You can not change the handle of a botmaster!"
    return 1 }
  if {[validuser $newn]} {
    putnotc $nick "There is allready a user known with handle $newn"
    return 1 }
  if {[chhandle $oldn $newn]} {
    putnotc $nick "Handle changed for $oldn. New handle: $newn"
    return 1
  } else { return 0 }
}

## ----------------------------------------------------------------
## --- TIMED STUFF					        ---
## ----------------------------------------------------------------

bind time - "00 * * * *" do_channels

proc do_channels {a b c d e} {
  global topicinfo cslogin bchan bnick userfile

  foreach a [channels] {
  if {![info exists topicinfo(ltopic$a)]} { set topicinfo(ltopic$a) "" }
  if {![info exists topicinfo(lwho$a)]} { set topicinfo(lwho$a) "" }}

  set fd [open $userfile.conf w]

  foreach chan [channels] {
  if {[info exists topicinfo(ltopic$chan)]} {
  puts $fd "$chan ltopic $topicinfo(ltopic$chan)"}
  if {[info exists topicinfo(lwho$chan)]} {
  puts $fd "$chan lwho $topicinfo(lwho$chan)"}
  if {[info exists topicinfo(greet$chan)]} {
  puts $fd "$chan greet $topicinfo(greet$chan)"}
  if {[ischanset $chan badnick] && [info exists bchan([string tolower $chan])]} {
  foreach ban $bchan([string tolower $chan]) {
  puts $fd "$chan bchan $ban" }}}

  putlog "#SaveConf# Configuration saved"
  close $fd 

  set purged 0
  foreach user [userlist] {
  if {[passwdok $user ""] && ![matchattr $user +b]} {
  deluser $user
  incr purged
  putlog "#CleanDB# Removed $user from database \(No Pass Set\)" }}
  putlog "#CleanDB# Cleaned up $purged users"
  save }

bind time - "*0 * * * *" check_idle_users
bind time - "*5 * * * *" check_idle_users

proc check_idle_users {a b c d e} {
  global botnick
  foreach chan [channels] {
  if {[ischanset $chan noopsidle]} {
  foreach user [chanlist $chan] {
  if {[isop $user $chan] && ![matchattr [nick2hand $user $chan] +b] && ![matchattr [nick2hand $user $chan] &A $chan] && [getchanidle $user $chan] > 20 && $user != $botnick } {
  pushmode $chan -o $user
  putnotc $user "Deopping after over 20 minutes of inactivity" }}}
  if {[ischanset $chan novoiceidle]} {
  foreach user [chanlist $chan] {
  if {[isvoice $user $chan] && ![matchattr [nick2hand $user $chan] +b] && ![matchattr [nick2hand $user $chan] &a $chan] && [getchanidle $user $chan] > 20 && $user != $botnick} {
  pushmode $chan -v $user
  putnotc $user "Devoicing after over 20 minutes of inactivity" }}}
  if {[ischanset $chan limiter]} {
  set currentcount [llength [chanlist $chan]]
  set curlimit [string range [getchanmode $chan] [expr [string last " " [getchanmode $chan]] + 1] end]
  if {$curlimit == [expr $currentcount +5]} { return 0 }
  incr currentcount 5
  pushmode $chan +l $currentcount}}
  }

proc loadconfig { } {
  global topicinfo cslogin bchan bword userfile
  if {![file exists $userfile.conf]} {return 0}
  set bchan(chans) ""
  set cfile [open $userfile.conf r]
  while {![eof $cfile]} {
  gets $cfile fconfline
  set chan [lindex $fconfline 0]
  set option [lindex $fconfline 1]
  set value [lrange $fconfline 2 end]
  if {$option == "ltopic"} { set topicinfo(ltopic$chan) $value }
  if {$option == "lwho"} { set topicinfo(lwho$chan) $value }
  if {$option == "greet"} { set topicinfo(greet$chan) $value }
  if {$option == "bchan"} { 
   if {![info exist bchan([string tolower $chan])]} {
   set bchan([string tolower $chan] $value
   } { 
   lappend bchan([string tolower $chan]) $value }
  }
  if {$option == "bword"} {
   if {![info exist bword([string tolower $chan])]} {
   set bword([string tolower $chan] $value
   } {
   lappend bword([string tolower $chan]) $value }
  }
  }
  close $cfile 
  putlog "Configuration for Multi loaded...." }

## ----------------------------------------------------------------
## --- RAW 005 Handler                                          ---
## ----------------------------------------------------------------
bind raw - 005 server_support
if {![info exists support(dummy)]} { set support(dummy) 1 }

proc server_support {from idx args} {
  global support max-modes max-bans nick-len
  set args [lrange [lindex $args 0] 1 end]
  foreach option $args {
  regsub -all "=" $option " " option
  switch [lindex $option 0] {
  MAXCHANNELS { set support(maxchan) [lindex $option 1] }
        MODES { set max-modes [lindex $option 1] }
      MAXBANS { set max-bans [lindex $option 1] }
      SILENCE { set support(silence) [lindex $option 1] }
      NICKLEN { set nick-len [lindex $option 1] }
     CPRIVMSG { set support(cprivmsg) 1 }
      CNOTICE { set support(cnotice) 1 }
     TOPICLEN { set support(topiclen) [lindex $option 1] }
      KICKLEN { set support(kicklen) [lindex $option 1] }
  }}}
  

## ----------------------------------------------------------------
## --- FLOODHANDLERS                                            ---
## ----------------------------------------------------------------
bind flud - * floodhandler
proc floodhandler { nick host hand type chan} {
  global support botnick
  set uhost [lindex [split $host @] 1]
  switch $type {
  
   msg { putnotc $nick "I don't like being flooded. Placing you on ignore."
         newignore [bmaskhost $host] $botnick "MSG Flood" 30
   	 if {[info exists support(silence)]} {
	 putserv "SILENCE +[bmaskhost $host]" 
	 timer 30 "putserv \"SILENCE -[bmaskhost $host]\"" }
	 putdebug "$nick \($host\) MSG Flooded me. Ignoring"
	 return 1 }
	 
  ctcp { putnotc $nick "CTCP Floods are evil. Placing you on ignore."
         newignore *@[lindex [split $host @] 1] $botnick "CTCP Flood" 30
	 newchanban $chan "*!*@$uhost" $botnick "CTCP Flooders \(30 minutes to cool down\)" 30
	 foreach user [chanlist $chan] {
	 if {[string match [string tolower *@$uhost *!*[string tolower [getchanhost $user $channel]]]} {
	 putkick $chan $user "CTCP Flooders \(30 minutes to cool down\)" }}
	 if {[info exists support(silence)]} {
	 putserv "SILENCE +*@$uhost"
	 timer 30 "putserv \"SILENCE -*!*@$uhost\"" }
	 putdebug "$uhost CTCP Flooded $chan. Taking action"
	 return 1 }

  join { putnotc $nick "Do not joinflood $chan. Try again to join in 10 minutes."
         newchanban $chan [bmaskhost $host] $botnick "Join Flooder \(10 minutes to cool down\)" 10
	 putnotc $chan "Join Flood detected. Temporary setting channel +i. Sorry for any inconvenience"
	 pushmode $chan +i
	 timer 1 "pushmode $chan -i"
	 putdebug "$uhost JOIN Flooded $chan. Taking action"
	 foreach user [chanlist $chan] {
	 if {[string match [string tolower *!*@$uhost *!*[string tolower [getchanhost $user $channel]]]} {
         putkick $chan $user "Join Flooder \(10 minutes to cool down\)" }}
	 return 1 }

   pub { putnotc $nick "Please do not flood in $chan. Try again in 5 minutes"
         newchanban $chan [bmaskhost $host] $botnick "Channel Flooder \(5 minutes to cool down\)" 5
	 putdebug "$nick \($host\) Flooded $chan. Taking action"
	 foreach user [chanlist $chan] {
	 if {[string match [string tolower [bmaskhost $host]] *!*[string tolower [getchanhost $user $chan]]]} {
	 putkick $chan $user "Channel Flooder \(5 minutes to cool down\)" }}
	 return 1 }
 }}

## ----------------------------------------------------------------
## --- BADCHAN                                                  ---
## ----------------------------------------------------------------
bind raw - 319 bc_whois 

proc bc_whois {from key args} {
  global bchan
  set args [join $args]
  set nick [string tolower [lindex $args 1]]
  set chans [string tolower [lrange $args 2 end]]
  
  foreach chan [channels] {
  if {![ischanset $chan badchan] || ![info exists bchan([string tolower $chan])] || ![onchan $nick $chan] } { continue }
  
  foreach tok $chans {
  set tok [string trimleft $tok ":@+"]
  
  foreach ban $bchan([string tolower $chan]) {
  if {[string match [lindex $ban 0] $tok]} {
  if {[onchan $nick $chan]} {
  putkick $chan $nick "[lrange $ban 1 e]"
  pushmode $chan +b [bmaskhost [getchanhost $nick $chan]]
  newchanban $chan [bmaskhost [getchanhost $nick $chan]] badchan "[lrange $ban 1 e]" 10 }}}}}}
  
## ----------------------------------------------------------------
## --- AUTOTOPIC                                                ---
## ----------------------------------------------------------------
bind time "00 * * * *" check_autotopic

proc check_autotopic {a b c d e} {
 foreach chan [channels] {
 if {[ischanset $chan autotopic] && [topic $chan] != "" } {
 putserv "TOPIC $chan :[topic $chan]" }}}

## ----------------------------------------------------------------
## --- BADWORD / NOREPEAT / NOCAPS / NONOTICE                   ---
## ----------------------------------------------------------------
bind pubm - * checkbword

proc checkbword {nick host hand chan args} {
  global bword botnick repeatcheck

  if {[string tolower $nick] == [string tolower $botnick] || [matchattr $hand &n] || [matchattr $hand +b]} {return 0}
  
  if {[ischanset $chan badword] && [info exists bword([string tolower $chan])]} {
  foreach word $bword([string tolower $chan]) {
  if {[string match [lindex [string tolower $word] 0] [string tolower $args]]} {
  pushmode $chan +b [bmaskhost $host]
  putkick $chan $nick "Profanity on $chan is not tolerated \[[lrange [string tolower $word] 1 end]\] \(1 minute to wash your mouth\)"
  newchanban $chan [bmaskhost $host] badword "Profanity on $chan is not tolerated \[[lrange [string tolower $word] 1 end]\] \(1 minute to wash your mouth\)" 1 
  return 0 }}}
	 
  if {[ischanset $chan norepeat]} {
  if {[info exists repeatcheck($nick!$host@$chan)] && $repeatcheck($nick!$host@$chan) == $args} {
  pushmode $chan +b [bmaskhost $host]
  putkick $chan $nick "Please do NOT repeat in $chan \(1 minute to calm down\)"
  newchanban $chan [bmaskhost $host] repeat "Please do NOT repeat in $chan \(1 minute to calm down\)" 1
  return 0 }
  set repeatcheck($nick!$host@$chan) $args }

  if {[ischanset $chan nocolour] && [string match *\x03* $args]} {
  pushmode $chan +b [bmaskhost $host]
  putkick $chan $nick "You are not allowed to use colours in $chan \(1 minute to calm down\)"
  newchanban $chan [bmaskhost $host] colour "You are not allowed to use colours in $chan \(1 minute to calm down\)" 1
  return 0 }

  if {[ischanset $chan nocaps]} {
  set len [string length $args] ; set cnt 0 ; set capcnt 0
  while {$cnt < $len} {
  if [isupper [string index $args $cnt]] { incr capcnt }
  incr cnt }
  if {$capcnt > 15 && [expr 100 * $capcnt / $len] > 60} {
  pushmode $chan +b [bmaskhost $host]
  putkick $chan $nick "Please do NOT use too many Caps in $chan \(1 minute to calm down\)"
  newchanban $chan [bmaskhost $host] caps "Please do NOT use too many Caps in $chan \(1 minute to calm down\)" 1
  return 0 }}
}

bind NOTC - * checkbnotice

proc checkbnotice {nick host hand args dest} {
  global botnick

  if {[string tolower $nick] == [string tolower $botnick] || [matchattr $hand &n] || [matchattr $hand +b]} {return 0}

  if {[validchan $dest] && [ischanset $dest nonotice]} {
  pushmode $dest +b [bmaskhost $host]
  putkick $dest $nick "You are not allowed to use notices in $dest \(1 minute to calm down\)"
  newchanban $dest [bmaskhost $host] notice "You are not allowed to use notices in $dest \(1 minute to calm down\)" 1
  return 0 }
}
## ----------------------------------------------------------------
## --- BOTNET						        ---
## ----------------------------------------------------------------
bind bot - $service botnet_proc
proc botnet_proc {bot cmd args} {
 global home service
 set chans [channels]
 set args [lindex $args 0]
 set blah [lindex $args 0]
 switch -exact $blah {
   "banner" { foreach chn $chans {
	      if {[string tolower $chn] != [string tolower $home]} {
              putmsg $chn "Global [lrange $args 2 end][lindex $args 1]" }}}
  "rehash"  { putdebug "Global rehash received from $bot"
              foreach timer [timers] {killtimer [lindex $timer 2]}
              rehash }
    "save"  { putdebug "Global save received from $bot"
              save }}}

## ----------------------------------------------------------------
## --- DCC Flagnote                                             ---
## ----------------------------------------------------------------
bind dcc m flagnote dcc_flagnote
proc dcc_flagnote {handle command arg} {
  set notes 0
  set toflag [lindex $arg 0]
  set msg [lrange $arg 1 end]
  if {[string index $toflag 0] == "+"} {
  set toflag [string index $toflag 1]
  if {$toflag == "b"} {
  putidx $command "You really think bot read notes ?"
  return 0 }}
  if {$toflag == "" || ($msg == "")} {
  putidx $command "Usage: .flagnote <flag> <message>"
  return 0 }
  putcmdlog "#$handle# flagnote +$toflag ..."
  foreach user [userlist] {
  if {![matchattr $user b] && [matchattr $user $toflag] && $user != $handle} {
  sendnote $handle $user "\[\002+$toflag\002\] :$msg"
  incr notes }}
  if {$notes == 0} {set notestring "no notes"}
  if {$notes == 1} {set notestring "1 note "}
  if {$notes >= 2} {set notestring "$notes notes have been"}
  putidx $command "Done... $notestring delivered!"}

## ----------------------------------------------------------------
## --- !shortnick                                               ---
## ----------------------------------------------------------------
bind pub - !shortnick pub_shortie
proc pub_shortie {nick host handle channel var} {
  global shortnick botnick
  putnotc $nick "I respond to $shortnick"
  return 1}

## ----------------------------------------------------------------
## --- !notify                                                  ---
## ----------------------------------------------------------------
bind pub m !notify pub_chanownnote
proc pub_chanownnote {nick host handle chan args} {
  global botnick
  set arg [lindex $args 0]
  set toflag [lindex $arg 0]
  set msg [lrange $arg 1 end]
  if {[validchan $toflag]} {
  putcmdlog "#$handle# chanownnote $toflag ..."
  foreach user [userlist |n $toflag] {
  sendnote $handle $user "\[\002+$toflag\002\] :$msg" }
  putnotc $nick "Note to chanowner of $toflag delivered"}}

## ----------------------------------------------------------------
## --- Colourkick                                               ---
## ----------------------------------------------------------------
bind ctcp - ACTION kick_actioncolor
proc kick_actioncolor {nick uhost hand dest keyword args} {
  if {![validchan $dest]} { return 0 }
  if {[string match *\x03* $args] && [ischanset $dest nocolour] && ![matchattr $hand &o $dest] && ![matchattr $hand o]} {
  putkick $dest $nick "You are not allowed to use colours in $dest" }}

## ----------------------------------------------------------------
## --- IRCEVENTS					        ---
## ----------------------------------------------------------------
bind ctcr - PING ping_me_reply
bind sign - * sign_deauth
bind kick - * prot_kick
bind mode - *-o* prot_deop
bind mode - *+b* prot_ban
bind mode - *+s* prot_sets
bind mode - *-s* prot_unsets
bind mode - *+l* prot_setl
bind topc - * topic_check
bind join - * join_handler 
bind nick Q * nick_change
bind evnt - init-server irc_init

proc irc_init { type } {
  global support
  if {[info exists support(silence)] && [llength [ignorelist]] != 0 } {
    putlog "Reinitializing ignores ..." 
    foreach ign [ignorelist] {
      set ihost [lindex $ign 0]; set iexpr [lindex $ign 2]
      putserv "SILENCE +$ihost"
      if {$iexpr != 0} { utimer [expr $iexpr - [unixtime]] "putserv \"SILENCE -$ihost\"" }
    }
  }}

proc ping_me_reply {nick uhost hand dest key arg} {
  if {![isnumber $arg]} {
    putnotc $nick "Nice try !"
    return 0
  }
  set dur [expr [unixtime] - $arg]
  putnotc $nick "Your ping reply took $dur seconds"
  return 0 }

proc sign_deauth {nick uhost hand chan rest} {
  if {[getuser [charfilter $hand] XTRA SECNICK] == [charfilter $nick] } {
  chattr $hand -Q
  setuser $hand XTRA SECNICK ""
  setuser $hand XTRA SECHOST "" }}

proc prot_kick {nick host hand chan knick reason} {
  global botnick home service kicklist
  set chan [string tolower $chan]
  set knick [string tolower $knick]
  set kickhand [nick2hand $knick $chan]
  if {$knick == [string tolower $botnick]} {
  putdebug "$nick kicked me from $chan stating $reason"
  putmsg $home "$nick kicked me from $chan stating $reason"
  putnotc $nick "Your kick of the bot has been logged"
  foreach user [userlist |n $chan] {
  sendnote "$serviceWarning" $user "$nick \[$host\] kicked me from $chan stating: $reason" }}
  if {$knick != [string tolower $botnick]} {
  set kicklist([charfilter $knick]@$chan) "1"
  utimer 3 "unset kicklist([charfilter $knick]@$chan)" }}

proc prot_sets {nick host hand chan mdechg amount} {
  channel set $chan +secret }

proc prot_unsets {nick host hand chan mdechg amount} {
  channel set $chan -secret }

proc prot_setl {nick host hand chan mdechg amount} {
  global botnick home service cservice
  if {$nick == $cservice} { return 0 }
  set chan [string tolower $chan]
  if {$amount <= [llength [chanlist $chan]]} {
  if {$nick=="$botnick"} { 
  pushmode $chan -l 
  return 0 } { 
  pushmode $chan -lo $nick }
  putnotc $nick "Do not try to set limits which would endanger the channel" }}

proc prot_deop {nick host hand chan mdechg dnick} {
  global botnick home service shortnick
  set deophand [nick2hand $dnick $chan]
  if {$dnick == $botnick} {
  if {$nick != ""} {
  putdebug "$nick deopped me on $chan"
  pushmode $chan +o $dnick
  putnotc $nick "Your deop of the bot has been logged"
  putmsg $home "$nick deopped me on $chan"}
  if {$nick == ""} {
  putdebug "Serverdeop from $host on $chan"
  putmsg $home "Serverdeop from $host on $channel" }}}

proc prot_ban {nick host hand chan mdechg dnick} {
  global home botname
  if {[string match $dnick $botname]} {
  pushmode $chan -ob $nick $dnick
  putnotc $nick "Protecting myself from getting banned"
  putmsg $home "Protecting myself from getting banned by $nick on $chan" }}

proc topic_check {nick uhost hand channel arg} {
  global topicinfo botnick
  if {$nick=="$botnick"} { return 0 }
  if {[ischanset $channel topiclock]} { 
  putserv "TOPIC $channel :$topicinfo(ltopic[string tolower $channel])"
  putnotc $nick "Sorry topic already locked by $topicinfo(lwho[string tolower $channel])." }
  return 1 }

proc nick_change {nick uhost hand chan rest} {
  if {[getuser $hand XTRA SECNICK] == [charfilter $nick]} {
  setuser $hand XTRA SECNICK [charfilter $rest] }
  if {![matchattr $hand &o $chan] && ![matchattr $hand o]} {
  if {[ischanset $chan nonickchange]} {
  pushmode $chan +b [bmaskhost $host]
  putkick $chan $nick "Please do not change nicks in $chan. 1 Minute ban"
  newchanban $chan [bmaskhost $host] $botnick "Please do not change nicks in $chan. 1 Minute ban" 1 
  return 0 }}
  return 1 }

proc join_handler {nick host handle channel} {
  global topicinfo botnick home kicklist shortnick bchan
  if {($nick == $botnick) || [matchattr $handle +b]} {return 0}
  
  if {![matchattr $handle &o $channel] && ![matchattr $handle o]} {
  
  if {[ischanset $channel norejoin] && [info exists kicklist([charfilter $nick]@[string tolower $channel])]} {
  pushmode $channel +b [bmaskhost $host]
  putkick $channel $nick "Disable Autorejoin. 1 Minute ban"
  newchanban $channel [bmaskhost $host] $botnick "Disable Autorejoin. 1 Minute ban" 1 
  return 0 }

  if {[ischanset $channel membersonly] && ![matchattr $handle &f $channel] } {
  pushmode $channel +b [bmaskhost $host]
  putkick $channel $nick "Only members are authorized to join $channel"
  newchanban $channel [bmaskhost $host] $botnick "Only members are authorized to join $channel" 30 
  return 0 }

  if {[ischanset $channel badchan] && ![matchattr $handle &f $channel] && [info exists bchan([string tolower $channel])]} {
  if {![info exists bchan($host![string tolower $channel])]} {
  putserv "WHOIS $nick" }}
  
  }

  if {[ischanset $channel greet] && [info exists topicinfo(greet$channel)]} {
  putnotc $nick [greetparse $nick $host $handle $channel $topicinfo(greet$channel)] }

  if {[ischanset $channel chanvoice]} {
  pushmode $channel +v $nick }

  if {[ischanset $channel chanop"]} {
  pushmode $channel +o $nick }

  if {[string tolower $channel] == [string tolower $home]} {return 0}
  foreach user [chanlist $channel] { if {[isop $user $channel]} {return 0}}
  putnotc $nick "This channel is opless, please type $shortnick ops to see who are authorized ops in this channel." }

## ----------------------------------------------------------------
## --- MSG Command Reroutes                                     ---
## ----------------------------------------------------------------
unbind msg - op *msg:op
unbind msg - voice *msg:voice
unbind msg n die *msg:die
unbind msg m rehash *msg:rehash
unbind msg m jump *msg:jump
unbind msg m save *msg:save
unbind msg - help *msg:help

set msgcoms1 "whoisall chattr rembadchan addbadchan listbadchan chanset fixmode del greet chanflood ctcpflood nickflood op deop topic act say banmask ban unban banlist kick mode voice devoice addinfo ops access count userlist matchcount verify getinfo searchinfo add chaninfo clean lock unlock purge cycle drop join part chanowner smack"

set msgcoms2 "gdel sethost addhost delhost nick jump die restart gchattr operlist rehash banner gbanner rehashall cycle drop away back save allbans msg shortnick server time date version queue uptime ping dbcount chat help chnick chhandle note"

foreach msg $msgcoms1 { bind msg - $msg msg_reroute }
foreach msg $msgcoms2 { bind msg - $msg msg_reroute1 }

proc msg_reroute {nick host hand args} {
 global botnick lastbind
 set args [lindex $args 0]
 set chan [lindex $args 0]
 set nicks [lrange $args 1 end]
 if {$chan == ""} {
 putnotc $nick "Usage: /msg $botnick $lastbind <channel> \[options\]"
 return 0 }
 if {![validchan $chan]} {
 putnotc $nick "I am not monitoring $chan"
 return 0 }
 pub_cd $nick $host $hand $chan "$lastbind $nicks"
 return 1 }

proc msg_reroute1 {nick host hand args} {
 global botnick lastbind home
 set args [lindex $args 0]
 pub_cd $nick $host $hand $home "$lastbind $args"
 return 1 }

## ----------------------------------------------------------------
## --- Main Command Crap				        ---
## ----------------------------------------------------------------
bind pub - $botnick pub_cd
bind pub - $shortnick pub_cd
bind pub o *** pub_cd

proc pub_cd {nick host handle channel var} {
 global network botname server version botnick home scriptversion shortnick contactchan topicinfo service uptime owner telnet support server-online bchan bword keep-nick cservice nickowner mailowner fcbversion
 set cmd [string tolower [lindex $var 0]]
 set who [string tolower [lindex $var 1]]
 set why [lrange $var 2 end]
 set oldwho [lindex $var 1]

 ## BotOwner Commands
 if {[matchattr $handle n] && [string tolower $owner] == [string tolower $handle]} {
 switch $cmd {
     gdel { if {![checksec $nick $host $handle]} { return 0 }
     	    if {$who == ""} {
	    putnotc $nick "Usage: gdel <handle>"
	    return 0 }
	    if {![validuser $who]} {
	    putnotc $nick "User $who does not exists in database"
	    return 0 }
	    if {[string tolower $who] == [string tolower $owner]} {
	    putnotc $nick "You cannot delete yourself"
	    return 0 }
	    deluser $who
	    putnotc $nick "Removed $who from the userdatabase"
	    putdebug "$nick deleted $who from the userdatabase"
	    return 1 }

 }
}


 ## Global Owner Commands
 if {[matchattr $handle n]} {
 switch $cmd {
  whoisall { if {![checksec $nick $host $handle]} { return 0 }
             putnotc $nick "Whoissing all users in $channel"
             foreach user [chanlist $channel] {
	     putserv "WHOIS $user" }
	     return 1 }

  sethost { if {![checksec $nick $host $handle]} { return 0 }
            set why [lindex $var 2]
            if {$who == ""} {
            putnotc $nick "Usage: sethost <handle> <hostmask>"
            return 0 }
            if {![validuser $who]} {
            putnotc $nick "User $who does not exists in database"
            return 0 }
            setuser $who HOSTS
            setuser $who HOSTS $why
            putnotc $nick "Changed host of $who to $why"
            return 1 } 

  addhost { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: addhost <handle> <hostmask>"
            return 0 }
            if {![validuser $who]} {
            putnotc $nick "User $who does not exists in database"
            return 0 }
	    if {![string match "*!*@*" $why]} {putnotc $nick "Invalid hostmask. Use format: *!*@*"
	    return 0 }
            setuser $who HOSTS $why
            putnotc $nick "Added host of $who to $why"
            return 1 }
	    
  delhost { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: delhost <handle> <hostmask>"
            return 0 }
            if {![validuser $who]} {
            putnotc $nick "User $who does not exists in database"
            return 0 }
	    if {[delhost $who $why]} {
	      putnotc $nick "Removed host $why from $who"
	    } else {
	      putnotc $nick "The hostmask $why does not exist for $who"
	    }
            return 1 }

     lock { if {![checksec $nick $host $handle]} { return 0 } 
            if {$who == ""} {
            putnotc $nick "Usage: lock <channel>"
            return 0 }
            putnotc $nick "Locking the channel $who."
            putmsg $who "!!! WARNING: Channel will be locked !!!"
            channel set $who chanmode "+stnmi"
	      pushmode $channel +stnmi
            set dorks [chanlist $who]
            foreach p $dorks {
            set victim [nick2hand $p $who]
            if {![matchattr $victim o] && $p != $botnick} {
            putkick $who $p "This channel has been locked"
            chattr $p |-o $who }}
            putserv "TOPIC $channel :Channel locked. Contact $contactchan for details"
            putdebug "$nick made me lock channel $who"}

     jump { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: jump <server>"
            return 0 }
            putnotc $nick "Jumping to $who"
            jump $who 
		putdebug "$nick made me jump to $who"}

      die { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
		putdebug "$nick made me shutdown myself... I\'M MELTING!!!"
            die $who }

  restart { if {![checksec $nick $host $handle]} { return 0 }
            putnotc $nick "Restarting"
            putdebug "$nick made me restart"
            restart }
 
  gchattr { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: gchattr <Changer Persons attributes.>"
            return 0 }
            chattr [nick2hand $who $channel] $why
            putnotc $nick "Added global attribute $why to $who."
            putdebug "$nick made me change $who's global attributes to $why."
            return 1 }
 
   unlock { if {![checksec $nick $host $handle]} { return 0 } 
            if {$who == ""} {
            putnotc $nick "Usage: unlock <channel>"
            return 0 }
            putnotc $nick "UnLocking the channel $who."
            channel set $who chanmode "+tn"
            pushmode $channel -smi
            putdebug "$nick made me unlock channel $who" 
            return 1 }
 }}

 ## Global Master Commands
 if {[matchattr $handle m]} {
 switch $cmd {
 operlist { if {![checksec $nick $host $handle]} { return 0 }
            putnotc $nick "Opers spotted by this bot:"
            foreach oper [userlist +O] {
             set lastontime [ctime [lindex [getuser $oper LASTON] 0]]
             set lastonchan [lindex [getuser $oper LASTON] 1]
             putnotc $nick " Nick: $oper \[[getuser $oper HOSTS]\] Last seen: $lastontime on $lastonchan"
            }}
	    
     note { if {![checksec $nick $host $handle]} { return 0 }
	    if {$who == ""} {
	    putnotc $nick "Usage: note <handle> <message>"
	    return 0}
	    if {![validuser $who]} {
	    putnotc $nick "I can't find anyone matching $who in the bot's userlist"
	    return 0 }
	    putnotc $nick "Sent note to $who: $why"
	    putmsg $home "\002$nick \002made me note\002 $who \002with: $why"
	    sendnote $nick $who $why
          }

   chnick { if {![checksec $nick $host $handle]} { return 0 }
	    if {$who == ""} {
	    putnotc $nick "Usage: chnick <old nick> <new nick>"
	    return 0}
	    set newn [string tolower [lindex $var 2]]
	    if {$newn == ""} {
	    putnotc $nick "Usage: chnick <old nick> <new nick>"
	    return 0}
	    if {![mchnick $nick $handle $who $newn]} {
	    putnotc $nick "Error changing handle of $who"
	    return 0}
	  }

 chhandle { if {![checksec $nick $host $handle]} { return 0 }
	    if {$who == ""} {
	    putnotc $nick "Usage: chnick <old nick> <new nick>"
	    return 0}
	    set newn [string tolower [lindex $var 2]]
	    if {$newn == ""} {
	    putnotc $nick "Usage: chnick <old nick> <new nick>"
	    return 0}
	    if {![mchnick $nick $handle $who $newn]} {
	    putnotc $nick "Error changing handle of $who"
	    return 0}
	  }

   rehash { if {![checksec $nick $host $handle]} { return 0 }
            putnotc $nick "Rehashing"
            putdebug "$nick made me rehash"
            foreach timer [timers] {killtimer [lindex $timer 2]}
            rehash }

   banner { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
	    putnotc $nick "Usage: banner <message>"
	    return 0 }
	    putnotc $nick "Sent banner: $who $why"
	    foreach chn [channels] {
	    putmsg $chn "GLOBAL $who $why \[$nick\]" }}

  gbanner { if {![checksec $nick $host $handle]} { return 0 } 
            if {$who == ""} {
            putnotc $nick "Usage: gbanner <message>"
            return 0 }
            putnotc $nick "Sent banner: $why"
            foreach chn [channels] {
            putmsg $chn "GLOBAL $who $why \[$nick\]" }
            putallbots "$service banner $nick $why" }

rehashall { if {![checksec $nick $host $handle]} { return 0 } 
            putnotc $nick "Rehashing all bots"
            putallbots "$service rehash"
            foreach timer [timers] {killtimer [lindex $timer 2]}
            rehash }

    purge { if {![checksec $nick $host $handle]} {return 0} 
            if {$who == ""} {
            putnotc $nick "Usage purge <channel>"
            return 0 }
	    putserv "PART $who :This channel has been purged on the request from $handle"
            channel remove $who
            putnotc $nick "Purged $who"
            putdebug "$nick made me purge $who" 
            return 1}

     away { if {![checksec $nick $host $handle]} {return 0}
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage: away <message>"
            return 0 }
            putserv "AWAY :$who"
            putnotc $nick "Bot is set to AWAY ($who)."
            putdebug "$nick made me put away ($who)."
            return 1 }

     back { if {![checksec $nick $host $handle]} {return 0}
            putserv "AWAY"
            putnotc $nick "Bot is set to BACK."
            putdebug "$nick made me put back."
            return 1 }

     save { if {![checksec $nick $host $handle]} {return 0} 
            save
            putnotc $nick "Saved."
            putdebug "$nick made me save." 
            return 1 }
 unignore { if {![checksec $nick $host $handle]} {return 0}
	    if {$who == ""} {
            putnotc $nick "Usage: unignore <host>"
            return 0 }
            if {![isignore $who]} {
            putnotc $nick "$who is not present in the ignorelist"
            return 0 }
            killignore $who
            if {[info exists support(silence)]} { putserv "SILENCE -$who" }
            putnotc $nick "$who has been removed from the ignorelist"
            return 1 }

  gbanlist { if {![checksec $nick $host $handle]} {return 0} 
            set banl [banlist]
            putnotc $nick "Global Banlist"
            if {$banl == ""} {
            putnotc $nick "Global Banlist is empty"
            return 0}
            foreach owns $banl {
            set hm [lindex $owns 0]
            set cm [lindex $owns 1]
            set ex [ctime [lindex $owns 2]]
            set ad [ctime [lindex $owns 3]]
            set cr [lindex $owns 5]
            putnotc $nick "Host: $hm Creator: $cr Added: $ad Expires: $ex Reason: $cm" }}
    gban {  if {![checksec $nick $host $handle]} {return 0}
	    set reas ""
	    if {$who == ""} {
	      putnotc $nick "Usage: gban <hostmask> \[reason\]"
	    return 0 }
	    if {$why == ""} { set reas "No reason supplied" }
	    newban $who [nick2hand $nick] $reas 0 sticky
	    putnotc $nick "Added $who to the global banlist"
	    putkick $channel $who "\($handle\) $why"
	    }
     ungban { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: ungban <mask>"
            return 0 }
            set banl [banlist]
            set banc 0
            foreach owns $banl {
            set hm [lindex $owns 0] 
            if {[string match [string tolower $who] [string tolower [lindex $owns 0]]]} {
		killban $hm
            foreach chn [channels] {
		pushmode $channel -b $hm
		incr banc }}}
		putnotc $nick "Removed $banc bans matching $who from the global banlist"
	    }
}
 ## Global Operator Commands
 if {[matchattr $handle o]} {
 switch $cmd {
      msg { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: msg <nick to msg> <What to say>"
            return 0 }
            putmsg $who "$why"
            putnotc $nick "Msg'd $who with :$why"
	    putmsg $home "\002$nick \002made me msg\002 $who \002with: $why"
            return 1 }
  gethost { if {$who == ""} {
            putnotc $nick "Usage: gethost <handle>"
            return 0 }
            if {![validuser $who]} {
            putnotc $nick "User does not exists in database"
            return 0 }
            putnotc $nick "$who has as host [getuser $who HOSTS]"
            return 0 }
 		}
	}
}

 ## Channel Owner Commands
 if {[matchattr $handle &n $channel] || [matchattr $handle o]} {
 switch $cmd {
  chattr {  if {![checksec $nick $host $handle] || [ischanset $channel suspend]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: chattr <Changer Persons attributes.>"
            return 0 }
            chattr [nick2hand $who $channel] |$why $channel
            putnotc $nick "Added attribute $why to $who on $channel."
            putdebug "$nick made me change $who's attributes to $why on $channel."
            putnotc $who "$nick changed your attributes on $channel to $why on $channel."
            return 1 }

    cycle { if {![checksec $nick $host $handle] || [ischanset $channel suspend]} { return 0 }
	    putserv "PART $channel :Cycling at the request of $nick" }
	    
    
     drop { if {![checksec $nick $host $handle] || [ischanset $channel suspend]} { return 0 }
            channel remove $channel 
            return 1 }

     join { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
	    putnotc $nick "Usage: join <channel>"
	    return 0 }
	    if {[validchan $who]} {
	    if {![matchattr $handle &n $who] && ![matchattr $handle o]} { return 0 }
	    if {[ischanset $who inactive]} {
	    if {[ischanset $who suspend]} {
	    putnotc $nick "$who has been suspended"
	    return 0 }
	    channel set $who -inactive
	    putnotc $nick "Rejoined $who"
	    putmsg $home "$nick made me rejoin $who"
	    return 1 }
	    putnotc $nick "I guess I am already on $who. Please look :-)"
	    return 0 }

	    if {![matchattr $handle +m]} {
	    putnotc $nick "Sorry but you don't have enough access to use this command"
	    return 0 }
	    if {[info exists support(maxchan)]} {
	    if {[llength [channels]] >= $support(maxchan)} {
	    putnotc $nick "Sorry but that would exceed my $support(maxchan) channel limit"
	    return 0 }}

	    channel add $who
	    
	    putnotc $nick "Joined $who"
	    putmsg $home "$nick made me join $who"
	    return 1 }

     part { if {![checksec $nick $host $handle] || [ischanset $channel suspend]} { return 0 }
	    if {$who == ""} {
	    putnotc $nick "Usage: part <channel>"
	    return 0 }
	    if {![matchattr $handle &n $who] && ![matchattr $handle o]} { return 0 }
	    putserv "PART $who :Parting at the request of $nick"
	    channel set $who +inactive
	    putnotc $nick "Left $who"
	    putmsg $home "$nick made me part $who"
	    return 1 }

 }}

 ## Channel Master Commands
 if {([matchattr $handle &m $channel] || [matchattr $handle o]) && ![ischanset $channel suspend]} {
 switch $cmd {

rembadword { if {![ischanset $channel badword]} { return 0 }
             if {![checksec $nick $host $handle]} { return 0 }
             if {$who == ""} {
             putnotc $nick "Usage: rembadword <chanmask>"
             return 0 }
             set temp ""; set found 0
             foreach ban $bword([string tolower $channel]) {
             if {[string match $who [lindex $ban 0]]} { incr found; continue } { lappend temp $ban }}
             set bword([string tolower $channel]) $temp
             putnotc $nick "$found items matching $who have been removed from $channel's badword list."
             return 1 }

addbadword { if {![ischanset $channel badword]} { return 0 }
             if {![checksec $nick $host $handle]} { return 0 }
	     if {$who == ""} {
             putnotc $nick "Usage: addbadword <wordmask> \[reason\]"
             return 0 }
             lappend bword([string tolower $channel]) "$who $why"
             putnotc $nick "$who was added to $channel's badword list."
             return 1 }
 
listbadword { if {![ischanset $channel badword]} { return 0 }
              if {![checksec $nick $host $handle]} { return 0 }
              if {![info exists bword([string tolower $channel])] || $bword([string tolower $channel]) == ""} {
              putnotc $nick "No badwords defined in $channel"
              return 0}
              putnotc $nick "BadWords for $channel"
              foreach ban $bword([string tolower $channel]) {
              putnotc $nick "$ban" }
              return 1 }

rembadchan { if {![ischanset $channel badchan]} { return 0 }
             if {![checksec $nick $host $handle]} { return 0 } 
	     if {$who == ""} {
	     putnotc $nick "Usage: rembadchan <chanmask>"
	     return 0 }
	     set temp ""; set found 0
	     foreach ban $bchan([string tolower $channel]) {
	     if {[string match $who [lindex $ban 0]]} { incr found; continue } { lappend temp $ban }}
	     set bchan([string tolower $channel]) $temp
	     putnotc $nick "$found items matching $who have been removed from $channel's badchan list."
	     return 1 }

addbadchan { if {![ischanset $channel badchan]} { return 0 } 
             if {![checksec $nick $host $handle]} { return 0 } 
	     if {$who == ""} {
	     putnotc $nick "Usage: addbadchan <chanmask> \[reason\]"
	     return 0 }
	     lappend bchan([string tolower $channel]) "$who $why"
   	     putnotc $nick "$who was added to $channel's badchan list."
	     return 1 }

listbadchan { if {![ischanset $channel badchan]} { return 0 } 
              if {![checksec $nick $host $handle]} { return 0 } 
	      if {![info exists bchan([string tolower $channel])] || $bchan([string tolower $channel]) == ""} {
	      putnotc $nick "No badchans defined in $channel"
	      return 0}
              putnotc $nick "BadChans for $channel"
	      foreach ban $bchan([string tolower $channel]) {
	      putnotc $nick "$ban" }
	      return 1 }

  chanset { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
	    set modes ""
            if {$who == ""} {
            putnotc $nick "Usage: chanset <settings>"
            return 0 }
	    foreach mode $who {
	    if {$mode == "+debug" && [string tolower $channel] == [string tolower $home] && [matchattr $handle +n]} {
	    set modes "$modes $mode"; continue }
	    if {$mode == "-debug" && [string tolower $channel] == [string tolower $home] && [matchattr $handle +n]} {
	    set modes "$modes $mode"; continue }
	    if {$mode == "+suspend" || $mode == "-suspend"} { continue }
	    if {$mode == "+topiclock"} {
	    set topicinfo(ltopic[string tolower $channel]) [topic $channel]
	    set topicinfo(lwho[string tolower $channel]) $handle
	    set modes "$modes $mode"; continue }
	    set modes "$modes $mode" }
	    foreach mode2 $modes { channel set $channel $mode2 }
            putnotc $nick "Changed settings on $channel to $modes."
            return 1 }

  fixmode { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage: fixmode <settings>"
            return 0 }
            channel set $channel chanmode $who
            putnotc $nick "Changed fixed mode on $channel to $who."
            putdebug "$nick made me change settings on $channel to chanmode $who." 
            return 1 }

    clean { if {![checksec $nick $host $handle]} { return 0 }
            set dorks [chanlist $channel]
            set dorklist ""
            set reason "$nick requested channel cleanup"
	      pushmode $channel +i
            foreach p $dorks {
            set who [nick2hand $p $channel]
            if {![matchattr $who o] && ![isop $p $channel] && ![isvoice $p $channel]} {
            append dorklist " " $p }}
            if {$dorklist == ""} {
            putnotc $nick "Couldn't find anyone needing kicking"
            return 0 }
            set blah "[llength $dorklist]"
            putnotc $nick "Kicking $blah Users: $dorklist"
            set count 0
            while {$count < $blah} {
            putkick $channel [lindex $dorklist $count] "$reason"
            incr count 1 }
	    pushmode $channel -i
            return 1 }

      del { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: del <nick>"
            return 0}
            if {[matchattr [nick2hand $who $channel] &n $channel]} {
            if {![matchattr [nick2hand $nick $channel] +o]} {
            putnotc $nick "You cannot delete the chanowner"
            return 0}}
            if {[matchattr [nick2hand $who $channel] o]} {
            putnotc $nick "You cannot delete a botmember"
            return 0}
            delchanrec [nick2hand $who $channel] $channel
            putnotc $nick "Removed $who from the channeldatabase"
            return 1 }

    greet { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage greet <message>"
            if {![info exists topicinfo(greet$channel)]} {
            putnotc $nick "Current setting: DISABLED" } else {
            putnotc $nick "Current setting: $topicinfo(greet$channel)" }
            return 0 }
            if {![ischanset $channel greet]} {
            putnotc $nick "Warning: You will need to use $shortnick chanset +greet in order to activate the greet" }
            set topicinfo(greet$channel) $who
            putnotc $nick "Greet set to $who"
            return 1 }

chanflood { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: chanflood <amount>"
            putnotc $nick " example: $shortnick chanflood 5 will kick when someone types 5 lines in 10 seconds."
            return 0 }
	    if {![isnumber $who]} {
	    putnotc $nick "Value should be a number."
	    return 0 }
	    if {$who == 0} { 
	    channel set $channel flood-chan 0:0
	    putnotc $nick "ChanFlood disabled" 
	    return 1 }
	    if {$who > 2 && $who < 11} {
	    channel set $channel flood-chan $who:10
	    putnotc $nick "ChanFlood set to $who"
	    return 1 }
	    putnotc $nick "Invalid value specified. Range = 3-10 & 0 to disable"
	    return 0 }

ctcpflood { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: ctcpflood <amount>"
            putnotc $nick " example: $shortnick ctcpflood 5 will kick when someone uses 5 ctcp's in 10 seconds."
            return 0 }
	    if {![isnumber $who]} {
	    putnotc $nick "Value should be a number."
	    return 0 }
	    if {$who == 0} {
	    channel set $channel flood-ctcp 0:0
	    putnotc $nick "CTCPFlood disabled"
	    return 1 }
	    if {$who > 2 && $who < 11} {
	    channel set $channel flood-ctcp $who:10
	    putnotc $nick "CTCPFlood set to $who"
	    return 1 }
	    putnotc $nick "Invalid value specified. Range = 3-10 & 0 to disable"
	    return 0 }

nickflood { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: nickflood <amount>"
            putnotc $nick " example: $shortnick nickflood 5 will kick when someone changing nicks 5 times within 1 minute"
            return 0 }
	    if {![isnumber $who]} {
	    putnotc $nick "Value should be a number."
	    return 0 }
	    if {$who == 0} {
	    channel set $channel flood-nick 0:0
	    putnotc $nick "NickFlood disabled"
	    return 1 }
	    if {$who > 2 && $who < 11} {
	    channel set $channel flood-nick $who:10
	    putnotc $nick "NickFlood set to $who"
	    return 1 }
	    putnotc $nick "Invalid value specified. Range = 3-10 & 0 to disable"
	    return 0 }
 }}

 ## Channel Operator Commands
 if {([matchattr $handle &o $channel] || [matchattr $handle o]) && ![ischanset $channel suspend]} {
 switch $cmd {

      say { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage: say <What to say>"
            return 0 }
            putmsg $channel "$who"
            putnotc $nick "Say'd $who"
            return 1 }

      act { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage: act <what to put as action>"
            return 0 }
            putmsg $channel "\001ACTION $who\001"
            putnotc $nick "Actioned $who"
            return 1 }

    topic { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "USAGE: topic <topic>"
            return 0 }
            if {[ischanset $channel topiclock]} {
            putnotc $nick "Sorry, the topic is locked"
            return 0 }
	    if {[info exists support(topiclen)]} {
	    if {[string length $who] > $support(topiclen)} {
	    putnotc $nick "Topic exceeds the limit of $support(topiclen) characters"
	    return 0 }}
            putserv "TOPIC $channel :$who"
            putnotc $nick "Topic on channel $channel set to $who"
            return 1 }

       op { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            pushmode $channel +o $nick
            return 1 }
            if {$who == "*"} {
	    putmsg $channel "MassOp issued by $nick \($handle\)"
            foreach user [chanlist $channel] {
            if {![isop $user $channel] && ![matchattr [nick2hand $user $channel] &d $channel]} {
	    pushmode $channel +o $user }}
            return 1 }
	    foreach victim "$who $why" {
	    if {![isop $victim $channel] && ![matchattr [nick2hand $victim $channel] &d $channel]} {
            pushmode $channel +o $victim 
	    putnotc $victim "You have been opped by $nick \($handle\)" }}
            return 1 }


     deop { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
	    pushmode $channel -o $nick
	    return 1 }
            foreach victim "$who $why" {
	    if {[string tolower $victim] != [string tolower $botnick] && [isop $victim $channel]} {
	    pushmode $channel -o $victim
	    putnotc $victim "You have been deopped by $nick \($handle\)" }}
	    return 1 }

  banmask { if {![checksec $nick $host $handle]} { return 0 }
            set why [lrange $var 3 end]
            set dur [lindex $var 2]
            if {$who == ""} {
            putnotc $nick "Usage: banmask <mask> <duration> <reason>"
            return 0 }
            newchanban $channel $who $nick "\($handle\) $why" $dur
            pushmode $channel +b $who
	    foreach user [chanlist $channel] {
	    if {[string match [string tolower $who] *!*[string tolower [getchanhost $user $channel]]]} {
	    putkick $channel $user "\($handle\) $why" }}
            putnotc $nick "Banned mask $who on $channel with reason: $why." 
            return 1 }

      ban { if {![checksec $nick $host $handle]} { return 0 }
            set why [lrange $var 3 end]
            set dur [lindex $var 2]
            set ban [bmaskhost [getchanhost $who $channel]]
            if {$who == ""} {
            putnotc $nick "Usage: ban <nick> <duration> <reason>"
            return 0 }
            if {![onchan $who $channel]} {
            putnotc $nick "$who aint on $channel."
            return 0 }
            if {[matchattr [nick2hand $who $channel] oQ]} {
            putnotc $nick "You cannot ban a botservice member"
            return 0 }
            if {$dur == ""} {
            set dur "10"}
            newchanban $channel $ban $nick "\($handle\) $why" $dur
  	      pushmode $channel -o $who
	      pushmode $channel +b $ban
	      putkick $channel $who "\($handle\) $why"
            putnotc $nick "Banned $who on $channel with reason: $why." 
            return 1 }

    unban { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            putnotc $nick "Usage: unban <mask>"
            return 0 }
            set banl [banlist $channel]
	    set banc 0
            foreach owns $banl {
	    set hm [lindex $owns 0]
            if {[string match [string tolower $who] [string tolower [lindex $owns 0]]]} {
	    killchanban $channel $hm 
	    pushmode $channel -b $hm
	    incr banc }}
            putnotc $nick "Removed $banc bans matching $who from $channel 's banlist" 
	    return 1 }

  banlist { if {![checksec $nick $host $handle]} { return 0 }
            set banl [banlist $channel]
            if {$banl == ""} {
            putnotc $nick "Banlist for $channel is empty"
            return 0}
            putnotc $nick "Banlist for $channel"
            foreach owns $banl {
            set hm [lindex $owns 0]
            set cm [lindex $owns 1]
            set ex [ctime [lindex $owns 2]]
            set ad [ctime [lindex $owns 3]]
            set cr [lindex $owns 5]
            putnotc $nick "Host: $hm Creator: $cr Added: $ad Expires: $ex Reason: $cm"}
            return 1 }

     kick { if {![checksec $nick $host $handle]} { return 0 }
            set why [lrange $var 2 end]
            if {$who == ""} {
            putnotc $nick "Usage: kick <nick to kick>"
            return 0 }
            if {![onchan $who $channel]} {
            putnotc $nick "$who isnt on $channel."
            return 0 }
            if {[string tolower $who] == [string tolower $botnick]} {
            putkick $channel $nick "hah. funny."
            return 0 }
            putkick $channel $who "\($handle\) $why"
            putnotc $nick "Kicked $who from $channel stating: $why." 
            return 1 }

     mode { if {![checksec $nick $host $handle]} { return 0 }
            set who [lrange $var 1 end]
            if {$who == ""} {
            putnotc $nick "Usage: mode <Channel mode you want to set>"
            return 0 }
            pushmode $channel $who 
            return 1 }

    voice { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            pushmode $channel +v $nick
            return 1 }
            if {$who == "*"} {
	    putmsg $channel "MassVoice issued by $nick \($handle\)"
            foreach user [chanlist $channel] {
            if {![isvoice $user $channel] && ![matchattr $user &q $channel]} {
	    pushmode $channel +v $user }}
	    return 1 }
	    foreach victim "$who $why" {
	    if {![isvoice $victim $channel]} {
	    pushmode $channel +v $victim
	    putnotc $victim "You have been voiced by $nick \($handle\)" }}
            return 1 }

  devoice { if {![checksec $nick $host $handle]} { return 0 }
            if {$who == ""} {
            pushmode $channel -v $nick
            return 1 }
            if {$who == "*"} {
	    putmsg $channel "MassDevoice issued by $nick \($handle\)"
            foreach user [chanlist $channel] {
            if {[isvoice $user $channel]} {
            pushmode $channel -v $user }}
	    return 1 }
	    foreach victim "$who $why" {
            if {[isvoice $victim $channel]} {
	    pushmode $channel -v $victim
	    putnotc $victim "You have been devoiced by $nick \($handle\)" }}
            return 1 }
 addinfo {  if {![checksec $nick $host $handle]} { return 0 }
 	    if {![ischanset $channel chandb]} { return 0 }
	      if {$who == ""} {
	      putnotc $nick "Usage: addinfo <keyword> <info2add>"
	      return 0}
	      set file [open "{[string tolower $network]}database.info" a]
	      puts $file "$channel $who [unixtime] $why"
	      close $file
	      putnotc $nick "$who added with $why"
	      return 1}
 ip { if {![checksec $nick $host $handle]} { return 0 }
        set ip [lindex [split $who] 0]	
        if {![ischanset $channel ip]} { return 0 }		
	if {![regexp {^(?:(?:[01]?\d?\d|2[0-4]\d|25[0-5])(\.|$)){4}$} $ip]} { putserv "PRIVMSG $channel :$nick NO/Invalid IP pattern. USAGE: $shortnick ip 123.4.56.7"; return }

	if {[catch {http::geturl "http://ip-api.com/json/$who"} tok]} {
		putlog "Socket error: $tok"
		return 0 }
	if {[http::status $tok] ne "ok"} {
	set status [http::status $tok]
	putlog "TCP error: $status"
	return 0 }
	if {[http::ncode $tok] != 200} {
	set code [http::code $tok]
	http::cleanup $tok
	putlog "HTTP Error: $code"
	return 0 }

	set connect [http::data $tok]
	set connect [json::json2dict $connect]
	set code [dict get $connect countryCode]
	set country [dict get $connect country]
	set regionname [dict get $connect region]
	set city [dict get $connect city]
	set isp [dict get $connect isp]
        set org [dict get $connect org]
        set timezone [dict get $connect timezone]
	putserv "PRIVMSG $channel :\002QueryIP\002-> \002IP\002\[$who\] \002City\002\[$city\] \002Country\002\[$code - $country\] \002TimeZone\002\[$timezone\] \002ISP\002\[$isp\] \002ORG\002\[$org\] <-\002QueryEND\002" }
 }}

 ## Channel Voice Commands
# if {([matchattr $handle &v $channel] || [matchattr $handle o]) && ![ischanset $channel suspend]} {
# switch $cmd {
#
# }}

 ## Public Commands
 switch $cmd {
    smack { set smacksizes "large big humongous huge monstrous giant"
            set smacksize [lindex $smacksizes [rand [llength $smacksizes]]]
	    set smackflavors "UNIX Caldera {Redhat 7.1} {Redhat 7.2} {Mandrake 8.0} Gentoo FreeBSD Suse Debian Slackware"
	    set smackflavor [lindex $smackflavors [rand [llength $smackflavors]]]
	    set smackamounts "infinitely {a bit} {a few times} {several times} {many times} {a lot} repeatedly continuously"
	    set smackamount [lindex $smackamounts [rand [llength $smackamounts]]]
	    if {$who == "" || $who == "mortician" || $who == $botnick} {
            putmsg $channel "\001ACTION smacks $nick upside the head $smackamount with a $smacksize $smackflavor manual\001"
            return 1 }
	    foreach victim "$who $why" {
	    putmsg $channel "\001ACTION smacks $who upside the head $smackamount with a $smacksize $smackflavor manual\001"}
            return 1 }

      ops { putnotc $nick "Authorized & Authenticated users to have ops in $channel :"
            if {[ischanset $channel suspend]} {
	    putnotc $nick "$channel has been suspended."
	    return 1}
            putnotc $nick "$botnick \(ChannelBot\)"
            set blah 1 
            foreach user [chanlist $channel] {
            if {[getuser [nick2hand $user $channel] XTRA SECHOST] == [getchanhost $user $channel] \
            && [getuser [nick2hand $user $channel] XTRA SECNICK] == $user } {
	    incr blah
	    putnotc $nick "$user \([getstatus $user $channel]\)" }}
            putnotc $nick "Detected a total of $blah authorized & authenticated ops" }

shortnick { putnotc $nick "My shortnick is $shortnick." }

   server { putnotc $nick "I'm currently on $server." }

     time { putnotc $nick "The current time is [time]" }

     date { putnotc $nick "Today is [date]" }

  version { putnotc $nick "eggdrop[lindex $version 0] :: Running Multi $scriptversion (c)2002 Key2Peace :: mod.FCbawt $fcbversion (c)2019 Rveyn" }

    queue { putnotc $nick "Current queue in bot:"
            putnotc $nick "Mode:[queuesize mode] Server:[queuesize server] Help:[queuesize server]" }

   uptime { putnotc $nick "Shell uptime : [exec uptime]"
            putnotc $nick "Bot Started  : [ctime $uptime]"
	    putnotc $nick "Online since : [ctime ${server-online}]"}

     ping { putmsg $nick "\001PING [unixtime]\001" }

    count { putnotc $nick "There are [llength [chanlist $channel]] users in $channel" }

  dbcount { putnotc $nick "$botnick has a database of [countusers] users \([llength [userlist +o]] Global Operators.\)" }

 chaninfo { if {[ischanset $channel suspend]} {
            putnotc $nick "$channel is suspended"
            return 0}
            set uc "0";set oc "0";set vc "0";set ul "";set dc "0"
            foreach p [chanlist $channel] {
            incr uc
            if {[isop $p $channel]} { incr oc; append ul " ^B@^B" $p } else {
            if {[isvoice $p $channel]} { incr vc; append ul " ^B+^B" $p } else {
            append ul " " $p }}}
            foreach p [userlist &f $channel] { incr dc }
            putnotc $nick "=-=-=-=-=-=-=-= $channel =-=-=-=-=-=-=-="
            if {[matchattr $handle o] && [checksec $nick $host $handle]} {
            putnotc $nick "Current chansets  : [lrange [channel info $channel] 19 57]"
		putnotc $nick "[lrange [channel info $channel] 58 61] [lrange [channel info $channel] 63 end]"
            putnotc $nick "Current chanmodes : [getchanmode $channel]"}
            putnotc $nick "Channel has a total of $uc user\(s\) \[$vc voiced / $oc opped\] currently in the channel"
            putnotc $nick "Channel contains $dc registered users"
            foreach co [userlist &n $channel] {
            putnotc $nick "Chanowner: $co (LastSeen: [ctime [lindex [getuser $co LASTON $channel] 0]])" }
            putnotc $nick "=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=" }

chanowner { if {[ischanset $channel suspend]} { 
            putnotc $nick "$channel is suspended"
	    return 0}
	    if {[userlist &n $channel] == ""} {
	    putnotc $nick "$channel does not seem to have a chanowner defined"
	    } {
	    foreach co [userlist &n $channel] {
	    putnotc $nick "Chanowner:$co LastSeen:[ctime [lindex [getuser $co LASTON $channel] 0]]" }}}
	    
 channels { set chans ""; set opless ""; set deact "";set suspd ""
            foreach chan [channels] {
            if {![ischanset $chan inactive]} {
	    if {[ischanset $chan secret] && [matchattr $handle o]} {
	      if {[onchan "$cservice" $chan]} {
	        append chans " \[[llength [chanlist $chan]]${cservice}S\]" $chan
	      } else {append chans " \[[llength [chanlist $chan]]S\]" $chan }
	    }
	    if {![ischanset $chan secret]} {
	      if {[onchan "$cservice" $chan]} {
	        append chans " \[[llength [chanlist $chan]]${cservice}\]" $chan
	      } else {append chans " \[[llength [chanlist $chan]]\]" $chan }
	    }}
            if {![isop $botnick $chan] && [botonchan $chan] && [string tolower $chan] != [string tolower $home] } {append opless " " $chan }
            if {[ischanset $chan inactive] && ![ischanset $chan suspend]} {append deact " " $chan}
	    if {[ischanset $chan inactive] && [ischanset $chan suspend]} {append suspd " " $chan}}
            putnotc $nick "I am currently on :$chans"
            if {$opless != ""} {putnotc $nick "Opless in: $opless"}
            if {$deact != ""} {putnotc $nick "Inactive on: $deact" } 
	    if {$suspd != ""} {putnotc $nick "Suspended on: $suspd" }
	    if {![info exists support(maxchan)]} {
	    putnotc $nick "Monitoring a total of [llength [channels]] channels" 
	    } { 
	    putnotc $nick "Monitoring a total of [llength [channels]] out of a maximum of $support(maxchan) channels" }}

 userlist { if {[ischanset $channel suspend]} {
            putnotc $nick "$channel is suspended"
	    return 0}
            putnotc $nick "Users present in $channel :"
            foreach p [chanlist $channel] {
            set appender "\[[getchanhost $p $channel]\] Idle: [getchanidle $p $channel] seconds."
            if {[isop $p $channel]} {
            putnotc $nick "@$p $appender" } else {
            if {[isvoice $p $channel]} {
            putnotc $nick "+$p $appender" } else {
            putnotc $nick "-$p $appender" }}}}

matchcount { if {$who == ""} {
             putnotc $nick "Usage: matchcount <mask>"
	     return 0}
	     set matches 0
	     foreach user [chanlist $channel] {
	     if {[string match [string tolower $who] [string tolower [getchanhost $user $channel]]]} { incr matches }}
	     putnotc $nick "A total of $matches users fit your mask \($who\)"
	     return 0 }

     chat {  if {![matchattr $handle p]} {
             putnotc $nick "I don't wanna chat with you"
	     return 0 }
	     if {[hand2idx $handle] != -1 } {
	     putnotc $nick "You are already on the partyline"
	     return 0 }
	     if {$telnet < "1024"} {
	     putnotc $nick "Sorry, your host isn't reachable at this time..."
	     return 0 }
	     putserv "PRIVMSG $nick :\001DCC CHAT chat [myip] $telnet\001"
	     putnotc $nick "DCC Chat - initializing.."
	     return 1 }

   verify { set user [nick2hand $who $channel]
            if {$who == ""} {
            putnotc $nick "Usage: verify <nick>"
            return 0}
            if {![onchan $who $channel]} {
            putnotc $nick "$who isnt on $channel."
            return 0}
            if {$user == "*"} {
            putnotc $nick "$who is unknown in the bot."
            return 0}
	    if {[ischanset $channel suspend]} {
	    putnotc $nick "$channel is suspended. So channel accesses won't work" }
            putnotc $nick "User is known in the bot as $user. Level: [getstatus $who $channel]"
	    return 1 }

  getinfo { if {[ischanset $channel suspend]} {
            putnotc $nick "$channel is suspended"
	    return 0}
	    if {![ischanset $channel chandb]} { return 0 } 
	    if {$who == ""} {
	    putnotc $nick "Usage: getinfo <keyword>"
	    return 0}
	    set file [open "{[string tolower $network]}database.info" r]
	    set lcnt 0
	    putnotc $nick "Grabbing info about $who"
	    while {![eof $file]} {
	    gets $file line
	    set dbchan [lindex [string tolower $line] 0]
	    set keywrd [lindex [string tolower $line] 1]
	    set created [ctime [lindex [string tolower $line] 2]]
	    set comment [lrange $line 3 end]
	    if {$keywrd == [string tolower $who] && $dbchan == [string tolower $channel]} {
	    incr lcnt
	    putnotc $nick "\[$created\] $comment" }}
	    putnotc $nick "A total of $lcnt lines found"
	    close $file 
	    return 1 }
 
searchinfo { if {[ischanset $channel suspend]} {
            putnotc $nick "$channel is suspended"
	    return 0}
	    if {![ischanset $channel chandb]} { return 0 } 
	    if {$who == ""} {
	    putnotc $nick "Usage: searchinfo <keyword>"
	    return 0}
	    set file [open "{[string tolower $network]}database.info" r]
	    set lcnt 0
	    putnotc $nick "Searching info about $who"
	    while {![eof $file]} {
	    gets $file line
	    set dbchan [lindex [string tolower $line] 0]
	    set created [ctime [lindex [string tolower $line] 2]]
	    set comment " [lindex [string tolower $line] 1] [lrange $line 3 end] "
	    if {[string match *[string tolower $who]* [string tolower $comment]] && $dbchan == [string tolower $channel]} {
	    incr lcnt
	    putnotc $nick "\[$created\][lindex $comment 0] [lrange $comment 1 end]" }}
	    putnotc $nick "A total of $lcnt lines found"
	    close $file 
	    return 1 }

     ghelp { if {![checksec $nick $host $handle]} {return 0}
            putserv "NOTICE $nick :Global ONLY Commands." 
            putserv "NOTICE $nick :\002" 
            if {[matchattr $handle n]} {
	    putserv "NOTICE $nick :\002Global Owner Commands:\002"
            putserv "NOTICE $nick :whoisall, sethost, addhost, delhost, lock, jump, die, restart, gchattr, unlock, gdel"
	    }
	    if {[matchattr $handle m]} {
	    putserv "NOTICE $nick :\002Global Master Commands:\002"
            putserv "NOTICE $nick :operlist, note, chnick, rehash, banner, gbanner, purge, away, back, save, unignore, gbanlist, gban, ungban"
	    }
	    if {[matchattr $handle o]} {
	    putserv "NOTICE $nick :\002Global ChanOP Commands:\002"
            putserv "NOTICE $nick :msg, gethost"
            putserv "NOTICE $nick : "
            putserv "NOTICE $nick :For basic user help do: $shortnick help"
	    }
           }
     lhelp { if {![checksec $nick $host $handle]} {return 0}
            putserv "NOTICE $nick :I'm a channel bot, intended primarily to prevent and defend against channel takeovers, Keep channel control." 
            putserv "NOTICE $nick :\002" 

	    if {[matchattr $handle &n $channel] || [matchattr $handle o]} {
	    putserv "NOTICE $nick :\002Channel Owner Commands:\002" 
            putserv "NOTICE $nick :chattr, cycle, drop, join, part"
	    }
	    if {[matchattr $handle &m $channel] || [matchattr $handle o]} {
	    putserv "NOTICE $nick :\002Channel Master Commands:\002"
            putserv "NOTICE $nick :addbadword, rembadword, listbadword, addbadchan, rambadchan, listbadchan, chanset, clean"
	    putserv "NOTICE $nick :add, del, greet, chanflood, ctcpflood, nickflood"
	    }
	    if {[matchattr $handle &o $channel] || [matchattr $handle o]} {
	    putserv "NOTICE $nick :\002Channel Op Commands:\002"
           putserv "NOTICE $nick :say, act, topic, op, deop, banmask, ban, unban, banlist, kick, mode, addinfo, IP"
	    }
            if {[matchattr $handle &v $channel] || [matchattr $handle o]} {
	    putserv "NOTICE $nick :\002Channel Voice Commands:\002" 
            putserv "NOTICE $nick :voice, devoice"
	    } }
     uhelp { if {![checksec $nick $host $handle]} {return 0} 
            putserv "NOTICE $nick :I'm a channel bot, intended primarily to prevent and defend against channel takeovers, Keep channel control." 
            putserv "NOTICE $nick :\002"
            putserv "NOTICE $nick :\002Public Channel Command:\002"
            putserv "NOTICE $nick :smack, ops, shortnick, server, time, date, version, uptime, ping, count"
	    putserv "NOTICE $nick :chaninfo, chanowner, channels, userlist, verify, lhelp, ghelp " 
            putserv "NOTICE $nick : "
            putserv "NOTICE $nick :For more help do: $shortnick <command>" } 
    help { putserv "NOTICE $nick :Hello $nick, I'm mod.FCbawt(\002F\002riendly \002C\002hannel \002Bawt\002 $fcbversion"
           putserv "NOTICE $nick :\002"
           putserv "NOTICE $nick :Please select the area you would like help with from the list below."
           putserv "NOTICE $nick :The help system uses the format: $shortnick <a>help"
           putserv "NOTICE $nick :----------- Help Modules <a>"
           putserv "NOTICE $nick :ghelp                  - Global Access Help Menu"
           putserv "NOTICE $nick :lhelp                  - Local Access Channel Help Menu"
           putserv "NOTICE $nick :uhelp                  - Any User Channel Help Menu"
           putserv "NOTICE $nick :----------- End"
           putserv "NOTICE $nick :Any problm? My owner is $nickowner <$mailowner> contact us anytime :-)" }

showignore { if {[ignorelist] == ""} {
             putnotc $nick "The ignorelist is empty"
             return 1 }
	     putnotc $nick "Ignorelist in $botnick :"
             foreach ign [ignorelist] {
             set ihost [lindex $ign 0]
             set iexpr [lindex $ign 2]
             if {$iexpr == 0} { set duration "Always \(Perm\)" } { set duration "for [duration [expr $iexpr - [unixtime]]]" }
	     putnotc $nick "$ihost $duration" }
	     putnotc $nick "End of ignores"
             return 1 }
	
      add { set mode [string tolower [lindex $var 2]]
            set noaddifsuspend " chanvoice chanbot chanop chanmaster chanowner "
            if {![checksec $nick $host $handle]} { return 0 }
            set hmsk [lindex $var 3]
            putlog "$hmsk"
            set ophost $hmsk
            if {$hmsk == "" || ![string match $ophost "$who![getchanhost $who $channel]"] } {
	    set ophost [bmaskhost [getchanhost $who $channel]] } { set ophost $hmsk }
            if {$mode == ""} {
            putnotc $nick "Usage: add <nick> <level> \[optional mask\]"
	    if {[matchattr $handle n] && [string tolower $owner] == [string tolower $handle]} {
	    putnotc $nick "Usage: <chanmode> can be chanvoice chanbot chanop chanmaster chanowner globalop master owner oper"
	    return 0}
            if {[matchattr $handle n]} {
            putnotc $nick "Usage: <chanmode> can be chanvoice chanbot chanop chanmaster chanowner globalop master "
            return 0}
            if {[matchattr $handle m]} {
            putnotc $nick "Usage: <chanmode> can be chanvoice chanbot chanop chanmaster chanowner globalop"
            return 0}
            if {[matchattr $handle o]} {
            putnotc $nick "Usage: <chanmode> can be chanvoice chanbot chanop chanmaster chanowner"
            return 0}
            if {[matchattr $handle |n $channel]} {
            putnotc $nick "Usage: <chanmode> can be chanvoice chanbot chanop chanmaster"
            return 0}
            if {[matchattr $handle |m $channel]} {
            putnotc $nick "Usage: <chanmode> can be chanvoice chanop"
            return 0} }

	    if {[ischanset $channel suspend] && ![string match $mode $noaddifsuspend]} {
	    putnotc $nick "$channel is suspended"
	    return 0}

            if {$who == $nick} {
            putnotc $nick "Yeah right... try again !"
            return 0 }
	    
            if {![onchan $who $channel]} {
            putnotc $nick "$who must be on the channel for this to work!"
            return 0 }
	    
            if {[nick2hand $who $channel] != "*"} {
            putnotc $nick "Nick already available as [nick2hand $who $channel]."
            putnotc $who "You have been recognized as [nick2hand $who $channel]. If this is not you. Please tell $nick about it"
            set who [nick2hand $who $channel] } else {
	    if {[validuser $who]} {
	    putnotc $nick "Unable to add $who to the database, there is already a user with that handle in the bot having hosts: [getuser $who HOSTS]"
	    return 0 }
	    adduser $who $ophost
	    putnotc $who "You have been newly added to this bot. To use your access please set a password"
	    putnotc $who "To set your password type /msg $botnick pass <thepasswordyouwant>, to login use /msg $botnick login <thepasswordyouset>" }
	    
	    switch $mode {
	     chanvoice { if {[matchattr $handle o] || [matchattr $handle &m $channel]} {
	                 chattr $who +h|+vf $channel
			 putnotc $nick "Added user as chanvoice of $channel to the bot"
			 putdebug "$nick added $who ($ophost) as chanvoice of $channel to the bot"
			 return 1 }}
	        chanop { if {[matchattr $handle o] || [matchattr $handle &m $channel]} {
		         chattr $who +h|+ovf $channel
			 putnotc $nick "Added user as chanop of $channel to the bot"
			 putdebug "$nick added $who ($ophost) as chanop of $channel to the bot"
			 return 1 }}
	    chanmaster { if {[matchattr $handle o] || [matchattr $handle &n $channel]} {
	                 chattr $who +h|+ovmf $channel
			 putnotc $nick "Added user as chanmaster of $channel to the bot"
			 putdebug "$nick added $who ($ophost) as chanmaster of $channel to the bot"
			 return 1 }}
	       chanbot { if {[matchattr $handle o] || [matchattr $handle &n $channel]} {
	                 chattr $who +h|+ovaf $channel
			 putdebug "$nick added $who ($ophost) as chanbot of $channel to the bot"
			 putnotc $nick "Added user as chanbot of $channel to the bot"
			 return 1 }}
	     chanowner { if {[matchattr $handle o]} {
                         chattr $who +h|+ovnmf $channel
                         putdebug "$nick added $who ($ophost) as chanowner of $channel to the bot"
                         putnotc $nick "Added user as chanowner of $channel to the bot"
                         return 1 }}
              globalop { if {[matchattr $handle m]} {
                         chattr $who +o
                         putdebug "$nick added $who ($ophost) as global operator to the bot"
                         putnotc $nick "Added user as global operator to the bot"
                         return 1 }}
                master { if {[matchattr $handle n]} {
                         chattr $who +m
                         putdebug "$nick added $who ($ophost) as master to the bot"
                         putnotc $nick "Added user as master to the bot"
                         return 1 }}
                 owner { if {[matchattr $handle n] && [string tolower $owner] == [string tolower $handle]} {
                         chattr $who +n
                         putdebug "$nick added $who ($ophost) as owner to the bot"
                         putnotc $nick "Added user as owner to the bot"
			 return 1 }}
                  oper { if {[matchattr $handle n] && [string tolower $owner] == [string tolower $handle]} {
                         chattr $who +O
                         putdebug "$nick added $who ($ophost) as oper to the bot"
                         putnotc $nick "Added user as oper to the bot"
			 return 1 }}
		}}
 }
}

loadconfig

putlog "Multi $scriptversion (C)2001 Key2Peace loaded :: (C) 2019 Rveyn - fix bug and remove / add stuff"



##############################################
#MOD.ADD - +/- chanset VOnJoin               #
#                                            #
# Voice user on join if channel set +vonjoin #
#                                            #
##############################################
set logversion "1.0"
setudef flag vonjoin

bind join -|- "* *" voicewhois


proc voicewhois {nickname hostname handle channel} {
global botnick
set ::logchannel $channel
set ::lognick $nickname
if { [channel get $channel vonjoin] } {
if {![botisop $channel]} { return 0 }
if {[onchan $nickname $channel]} {
putquick "MODE $::logchannel +v $nickname" ;putlog "VoiceOnJoin: I voice $::lognick on $channel (Channel set to +vonjoin)"
}
}
}

putlog "Multi.tcl - MOD-ADD: VOnJoin $logversion by Rveyn"


