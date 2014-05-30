(define (send-mail request to from subj text)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (define smtp-host "smtp.MY.DOMAIN.NET")   ;;; You must change this to your smtp host !!!!!!
   (define smtp-protocol "imap")               ;;; and change this to your protocol 
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (getRealPath servlet File)
        (.getRealPath (.getServletContext (.getServletConfig servlet)) File))

  (define (send to from subj text)
   (define (isnull? x)  (or (equal? x "") (eq? x #null)))

   (define smtp-mbox "INBOX")
   (define props 
    (let ((x (java.lang.System.getProperties)))
       (.put x "mail.smtp.host" smtp-host)
       x))
   (define ses (javax.mail.Session.getDefaultInstance props #null))

   (define (sendmail to from subj text msg)
	(begin
	  (if (not (isnull? to))
	      (begin
		(let ((toAddrs (javax.mail.internet.InternetAddress.parse to #f))
		      (myto javax.mail.Message$RecipientType.TO$))
  		(.setRecipients msg myto toAddrs))))
	  (if (not (isnull? from))
	      (begin
		(let ((fromAddr (javax.mail.internet.InternetAddress. from)))
  		(.setFrom msg fromAddr))))
          (if (not (isnull? subj))
	      (.setSubject msg subj))
	  (if (not (isnull? text))
	      (.setText msg text))
	  (javax.mail.Transport.send msg)))
   (sendmail to from subj text (javax.mail.internet.MimeMessage. ses)))

;  (let ((f (java.io.PrintWriter. (java.io.FileWriter. "mail-cache" #t))))
;    (.println f 
;       (list 
;          (java.util.Date.)
;          (.getServletPath request)
;          to from subj text))
;    (.close f))

  (send to from subj (.toString text)) 
)
