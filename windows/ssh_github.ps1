ssh-keygen -t ed25519 -C "weitingchen0502@gmail.com"
cat ~/.ssh/id_ed25519.pub
# have to add the public key to github account
ssh -T git@github.com
