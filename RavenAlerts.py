#!/usr/bin/env python

import os
from email.mime.text import MIMEText 
import smtplib

stuff=os.popen('sensors').read()

msg=MIMEText(stuff)

msg['Subject']='test'
msg['From']='chem332@raven'
msg['To']='normnite@gmail.com'

s=smtplib.SMTP('localhost')
print 'hi'
s.sendmail('chem332@raven.stthomas.edu',['normnite@gmail.com'],msg.as_string())
s.quit()

