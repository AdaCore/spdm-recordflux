#!/bin/bash

set -e

cd $1

openssl genrsa -out TestRoot.key 2048
openssl req -extensions v3_ca -new -x509 -days 3650 -key TestRoot.key -out TestRoot.crt -subj "/C=DE"
openssl x509 -in TestRoot.crt -out TestRoot.cer -outform DER
openssl x509 -inform DER -in TestRoot.cer -outform PEM -out TestRoot.pub.pem

mkdir -p rsa3072
pushd rsa3072
openssl req -nodes -x509 -days 3650 -newkey rsa:4096 -keyout ca.key -out ca.cert -sha384 -subj "/CN=intel test RSA CA"
openssl rsa -in ca.key -outform der -out ca.key.der
openssl req -nodes -newkey rsa:3072 -keyout inter.key -out inter.req -sha384 -batch -subj "/CN=intel test RSA intermediate cert"
openssl req -nodes -newkey rsa:3072 -keyout end_requester.key -out end_requester.req -sha384 -batch -subj "/CN=intel test RSA requseter cert"
openssl req -nodes -newkey rsa:3072 -keyout end_responder.key -out end_responder.req -sha384 -batch -subj "/CN=intel test RSA responder cert"
openssl x509 -req -in inter.req -out inter.cert -CA ca.cert -CAkey ca.key -sha384 -days 3650 -set_serial 1 -extensions v3_inter -extfile ../openssl.cnf
openssl x509 -req -in end_requester.req -out end_requester.cert -CA inter.cert -CAkey inter.key -sha384 -days 365 -set_serial 2 -extensions v3_end -extfile ../openssl.cnf
openssl x509 -req -in end_responder.req -out end_responder.cert -CA inter.cert -CAkey inter.key -sha384 -days 365 -set_serial 3 -extensions v3_end -extfile ../openssl.cnf
openssl asn1parse -in ca.cert -out ca.cert.der
openssl asn1parse -in inter.cert -out inter.cert.der
openssl asn1parse -in end_requester.cert -out end_requester.cert.der
openssl asn1parse -in end_responder.cert -out end_responder.cert.der
cat ca.cert.der inter.cert.der end_requester.cert.der > bundle_requester.certchain.der
cat ca.cert.der inter.cert.der end_responder.cert.der > bundle_responder.certchain.der
openssl rsa -inform PEM -outform DER -in end_responder.key -out end_responder.key.der
openssl rsa -inform PEM -outform DER -in end_requester.key -out end_requester.key.der
popd

mkdir -p ecp384
pushd ecp384
openssl genpkey -genparam -out param.pem -algorithm EC -pkeyopt ec_paramgen_curve:P-384
openssl req -nodes -x509 -days 3650 -newkey ec:param.pem -keyout ca.key -out ca.cert -sha384 -subj "/CN=intel test ECP256 CA"
openssl pkey -in ca.key -outform der -out ca.key.der
openssl req -nodes -newkey ec:param.pem -keyout inter.key -out inter.req -sha384 -batch -subj "/CN=intel test ECP256 intermediate cert"
openssl req -nodes -newkey ec:param.pem -keyout end_requester.key -out end_requester.req -sha384 -batch -subj "/CN=intel test ECP256 requseter cert"
openssl req -nodes -newkey ec:param.pem -keyout end_responder.key -out end_responder.req -sha384 -batch -subj "/CN=intel test ECP256 responder cert"
openssl x509 -req -in inter.req -out inter.cert -CA ca.cert -CAkey ca.key -sha384 -days 3650 -set_serial 1 -extensions v3_inter -extfile ../openssl.cnf
openssl x509 -req -in end_requester.req -out end_requester.cert -CA inter.cert -CAkey inter.key -sha384 -days 365 -set_serial 2 -extensions v3_end -extfile ../openssl.cnf
openssl x509 -req -in end_responder.req -out end_responder.cert -CA inter.cert -CAkey inter.key -sha384 -days 365 -set_serial 3 -extensions v3_end -extfile ../openssl.cnf
openssl asn1parse -in ca.cert -out ca.cert.der
openssl asn1parse -in inter.cert -out inter.cert.der
openssl asn1parse -in end_requester.cert -out end_requester.cert.der
openssl asn1parse -in end_responder.cert -out end_responder.cert.der
cat ca.cert.der inter.cert.der end_requester.cert.der > bundle_requester.certchain.der
cat ca.cert.der inter.cert.der end_responder.cert.der > bundle_responder.certchain.der
openssl ec -inform PEM -outform DER -in end_responder.key -out end_responder.key.der
openssl pkcs8 -in end_responder.key.der -inform DER -topk8 -nocrypt -outform DER > end_responder.key.p8
openssl ec -inform PEM -outform DER -in end_requester.key -out end_requester.key.der
openssl pkcs8 -in end_requester.key.der -inform DER -topk8 -nocrypt -outform DER > end_requester.key.p8
popd
