~/.profile;
set -x EC2_HOME /Users/rls/util/ec2-api-tools-1.7.5.1
set -x EC2_CERT "set this to the path to the certificate assigned to EC2 account"
set -x EC2_PRIVATE_KEY "set this to the path to the private key assigned to your AWS account"
set -gx PATH ~/bin $PATH
set -gx CDPATH ~/code .
