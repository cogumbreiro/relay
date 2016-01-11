
#!/bin/bash

#location of a RSA key for ssh v.2
SSH_DIR="/home/jan/.ssh"

KEY_NAME="id_rsa"

KEY_FILE=$SSH_DIR/$KEY_NAME

AUTHKEYS_FILE=$SSH_DIR/"authorized_keys"

#copy your key to these hosts
TARGETHOSTS="meatwad.ucsd.edu individual.ucsd.edu"

#------

ssh-keygen -f $KEY_FILE -t rsa

for h in $TARGETHOSTS; do
    echo "COPYING to $h"
    scp $KEY_FILE".pub" $h:.
    echo "APPENDING to authorized_keys then removing the copied key"
    ssh $h "(test -d $SSH_DIR || (mkdir $SSH_DIR && chmod 700 $SSH_DIR)) && cat $KEY_NAME.pub >> $AUTHKEYS_FILE && chmod 600 $AUTHKEYS_FILE && rm $KEY_NAME.pub"
    echo "DONE!"
done


