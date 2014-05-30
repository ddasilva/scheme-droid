;;
;; ** to run in the Demorunner, press "eval"
;;


(import "java.awt.*")
(import "java.awt.event.*")
(import "java.io.*")
(import "java.lang.*")
(import "java.lang.reflect.*")
(import "javax.swing.*")
(import "javax.swing.tree.*")
(import "java.util.*")
(import "jsint.*")

;;; File walking stuff from build/bootstrap.jsint.
(import "java.io.File")
(import "java.util.jar.JarFile")

(load "elf/basic.scm")

(define (file-walk file how so-far)
  (define (file-walk-files files how so-far)
    (for-each (lambda (f) (set! so-far (file-walk f how so-far)))
	      (vector->list files))
    so-far)
  (if (.isDirectory file)
      (file-walk-files (.listFiles file) how so-far)
      (how file so-far)))

(define-method (toFile (file String)) (File. file))
(define-method (toFile (file File)) file)
(define-method (toFile (file Object)) (U.typeError "File" file))

(define (files file test)
  ;; Return all the files under and including file that pass (test file).
  (file-walk (toFile file)
	     (lambda (f so-far) (if (test f) (cons f so-far) so-far))
	     '()))

(define (file-type? file suffix)
  (.endsWith (.toString file) suffix))
;(define-method (file-type? (file java.io.File) suffix)
;  (.endsWith (.getName file) suffix))
;(define-method (file-type? file suffix) #f)

(define (java-file? file) (file-type? file ".java"))
(define (class-file? file) (file-type? file ".class"))
(define (scheme-file? file) (or (file-type? file ".scm")))
(define (jar-file? file) (file-type? file ".jar"))

(define separator (java.lang.System.getProperty "path.separator"))

(define path 
  (string-append 
    (java.lang.System.getProperty "sun.boot.class.path")
    separator
    (java.lang.System.getProperty "java.class.path")
    separator
    (java.lang.System.getProperty "java.ext.dirs")
))


(define path-files (map File. (crack path separator)))

(define (classes files so-far)
  (define (classes0 file files so-far)
    (if (.exists file)
	(classes files ((if (jar-file? file) jar-classes
			    directory-classes)
				file so-far))
	(classes files so-far)))
  (if (null? files) so-far
      (classes0 (car files) (cdr files) so-far)))

(define (file-name->class f)
  (.substring f 0 (- (.length f) (.length ".class"))))


(define (jar-classes file so-far)
  (let loop ((fs (.entries (JarFile. file))))
    (if (.hasMoreElements fs)
	(let ((f (.nextElement fs)))
             (invokeAndWait (lambda() (let ((x (readexpr progress-ta)))
                   (writeexpr progress-ta {[(+ 1 x)] [f]\n}))))
	  (if (class-file? f)
	      (set! so-far (cons (map Symbol.intern
				      (crack (file-name->class (.getName f))
					     "/"))
				 so-far)))
	  (loop fs))))
  so-far)


(define (directory-classes file so-far)
  (let* ((name (.toString file))
	 (L (+ (.length name) 1))
	 (sep (System.getProperty "file.separator")))
    (if (.isDirectory file)
	(begin
	  (for-each
	   (lambda (f)
             (invokeAndWait (lambda() (let ((x (readexpr progress-ta)))
                   (writeexpr progress-ta {[(+ 1 x)] [f]\n}))))
	     (set! so-far
		   (cons
		    (map Symbol.intern
			 (crack (file-name->class (.substring (.toString f) L))
				sep))
		    so-far)))
	   (files file class-file?))
	  (for-each
	   (lambda (f)
             (invokeAndWait (lambda() (let ((x (readexpr progress-ta)))
                   (writeexpr progress-ta {[(+ 1 x)] [f]\n}))))
              (if (jar-file? f) 
                 (begin 
                   (set! so-far (jar-classes f so-far)))))
           (files file jar-file?))

	  so-far)
         so-far)))

 (define classlist #null)

 (load "jlib/JLIB.scm")
 (define progress-ta (label "0" yellow (CourierBold 12)))
 (define progress-win (window "progress"
    (border (north (label "Loading Classes from Classpath" (HelveticaBold 30))) (center progress-ta))))
 (define(invokeAndWait F)   ;; this is needed when interacting with Swing components from a separate thread
     (javax.swing.SwingUtilities.invokeAndWait (lambda()
         (tryCatch 
             (F)
             (lambda(e) (printdebug 'error {ERROR in invokeAndWait: [F], [e]\n})))
     ))  )

 (define computing-classlist 
  (begin
    (.pack progress-win)
    (.show progress-win)
    (.start 
     (java.lang.Thread. 
      (lambda() 
        (set! classlist (classes path-files '()))
        (synchronize progress-ta .notify)
       )))
    (display "waiting\n")
    (synchronize progress-ta .wait)
    (display "done waiting\n")
 ))


; (define classlist (classes path-files '()))
;(time (begin (set! classlist (classes path-files '())) (length classlist)) 1)
; (15007 (23925 msec) (-1298232 bytes))

(define (make-classtree classlist tree)
  (define (insert x t)
    (if (= (length x) 1)
	(cons (first x) t)
	(insert-elt (first x) (rest x) t)))
  (define (insert-elt a b t)
    (cond ((null? t)
	   (list 
	    (list a
		  (insert b ()))))
	  ((not (pair? (first t)))
	   (cons (first t)
		 (insert-elt a b (rest t))))
	  ((equal? a (first (first t)))
	   (cons
	    (list 
	     (first (first t)) 
	     (insert b (second (first t))))
	    (rest t)))
	  (else
	   (cons (first t)
		 (insert-elt a b (rest t))))))

  (if (null? classlist) tree
      (make-classtree (rest classlist) (insert (first classlist) tree	))))

(define (rcons it a b)
  (if (and (eq? a (car it)) (eq? b (cdr it)))
      (begin (print 'win) it)
      (cons a b)))

(define c-null 0)
(define c-not-pair 0)
(define c-eq 0)
(define c-else 0)

(define (reset)
  (set! c-null 0)
  (set! c-not-pair 0)
  (set! c-eq 0)
  (set! c-else 0))

(define (report)
  (list c-null c-not-pair c-eq c-else))

(define (insert-elt a b t)
  (cond
   ((null? t)				;  0.02%
    (list (list a (insert-empty b))))
   ((not (pair? (first t)))		; 85.3%
    (cons (first t) (insert-elt a b (rest t))))
   ((eq? a (first (first t)))		; was equal? 3.2%
    (cons
     (list 
      (first (first t)) 
      (insert b (second (first t))))
     (rest t)))
   (else				; 11.5%
    (cons (first t)
	  (insert-elt a b (rest t))))))

(define (insert x t)
    (if (null? t) (insert-empty x)
	(if (= (length x) 1)
	    (cons (first x) t)
	    (insert-elt (first x) (rest x) t))))
(define (insert-empty x)
  (if (= (length x) 1)
      (cons (first x) '())
      (list (list (first x) (insert-empty (rest x))))))

(define (make-classtree classlist tree)
  (if (null? classlist) tree
      (make-classtree (rest classlist) (insert (first classlist) tree	))))

;;; A tree is a list of branches. tree = `(,branch . tree)
;;; A branch = `(,name  ,tree)

(define tree-branch car)
(define tree-rest cdr)
(define branch-name car)
(define branch-tree cadr)

(writeexpr progress-ta {0 Creating classtree})
(define classtree (make-classtree classlist ()))
;;; KRA 19AUG00: This takes 70% of the 1.9 minutes! 80.386 sec.
;;; (time (begin (set! classtree (make-classtree classlist '())) 'done) 1)
;;; (done (81417 msec) (784624 bytes))
;;; (make-classtree '((a b c) (a b d) (a e f)) '())
;;; with insert-empty (done (80826 msec) (684840 bytes))
'(define classtree '(
(com(
    (sun(
        (image(
            (codec(
                (jpeg(
                    ImageFormatException
                    JPEGCodec
                    JPEGDecodeParam
                    JPEGEncodeParam
                    JPEGHuffmanTable
                    JPEGImageDecoder
                    JPEGImageEncoder
                    JPEGQTable
                    TruncatedFileException
                    ))
                ))
            ))
        (swing(
            (plaf(
                (motif(
                    MotifBorders
                    MotifButtonListener
                    MotifButtonUI
                    MotifCheckBoxMenuItemUI
                    MotifCheckBoxUI
                    MotifComboBoxRenderer
                    MotifComboBoxUI
                    MotifDesktopIconUI
                    MotifDesktopPaneUI
                    MotifEditorPaneUI
                    MotifFileChooserUI
                    MotifGraphicsUtils
                    MotifIconFactory
                    MotifInternalFrameTitlePane
                    MotifInternalFrameUI
                    MotifLabelUI
                    MotifLookAndFeel
                    MotifMenuBarUI
                    MotifMenuItemUI
                    MotifMenuMouseListener
                    MotifMenuMouseMotionListener
                    MotifMenuUI
                    MotifOptionPaneUI
                    MotifPasswordFieldUI
                    MotifPopupMenuSeparatorUI
                    MotifPopupMenuUI
                    MotifProgressBarUI
                    MotifRadioButtonMenuItemUI
                    MotifRadioButtonUI
                    MotifScrollBarButton
                    MotifScrollBarUI
                    MotifScrollPaneUI
                    MotifSeparatorUI
                    MotifSliderUI
                    MotifSplitPaneDivider
                    MotifSplitPaneUI
                    MotifTabbedPaneUI
                    MotifTextAreaUI
                    MotifTextFieldUI
                    MotifTextPaneUI
                    MotifTextUI
                    MotifToggleButtonUI
                    MotifTreeCellRenderer
                    MotifTreeUI
                    ))
                (windows(
                    WindowsBorders
                    WindowsButtonListener
                    WindowsButtonUI
                    WindowsCheckBoxMenuItemUI
                    WindowsCheckBoxUI
                    WindowsComboBoxUI
                    WindowsDesktopIconUI
                    WindowsDesktopManager
                    WindowsDesktopPaneUI
                    WindowsEditorPaneUI
                    WindowsFileChooserUI
                    WindowsIconFactory
                    WindowsInternalFrameUI
                    WindowsLabelUI
                    WindowsListUI
                    WindowsLookAndFeel
                    WindowsMenuBarUI
                    WindowsMenuItemUI
                    WindowsMenuUI
                    WindowsOptionPaneUI
                    WindowsPasswordFieldUI
                    WindowsPopupMenuUI
                    WindowsProgressBarUI
                    WindowsRadioButtonMenuItemUI
                    WindowsRadioButtonUI
                    WindowsScrollBarUI
                    WindowsScrollPaneUI
                    WindowsSeparatorUI
                    WindowsSliderUI
                    WindowsSpinnerUI
                    WindowsSplitPaneDivider
                    WindowsSplitPaneUI
                    WindowsStandardDialogUI
                    WindowsTabbedPaneUI
                    WindowsTableHeaderUI
                    WindowsTableUI
                    WindowsTextAreaUI
                    WindowsTextFieldUI
                    WindowsTextPaneUI
                    WindowsTextUI
                    WindowsToggleButtonUI
                    WindowsToolBarUI
                    WindowsTreeUI
                    WindowsUtils
                    ))
                ))
            ))
        ))
    ))
(java(
    (applet(
        Applet
        AppletContext
        AppletStub
        AudioClip
        ))
    (awt(
        AWTError
        AWTEvent
        AWTEventMulticaster
        AWTException
        AWTPermission
        ActiveEvent
        Adjustable
        AlphaComposite
        AlphaCompositeContext
        BasicStroke
        BorderLayout
        Button
        Canvas
        CardLayout
        Checkbox
        CheckboxGroup
        CheckboxMenuItem
        Choice
        Color
        ColorPaintContext
        Component
        ComponentOrientation
        Composite
        CompositeContext
        Conditional
        Container
        Cursor
        Dialog
        Dimension
        Event
        EventDispatchThread
        EventQueue
        FileDialog
        FlowLayout
        Font
        FontMetrics
        Frame
        GradientPaint
        GradientPaintContext
        Graphics
        Graphics2D
        GraphicsConfigTemplate
        GraphicsConfiguration
        GraphicsDevice
        GraphicsEnvironment
        GridBagConstraints
        GridBagLayout
        GridLayout
        IllegalComponentStateException
        Image
        Insets
        ItemSelectable
        Label
        LayoutManager
        LayoutManager2
        List
        MediaTracker
        Menu
        MenuBar
        MenuComponent
        MenuContainer
        MenuItem
        MenuShortcut
        Paint
        PaintContext
        Panel
        Point
        Polygon
        PopupMenu
        PrintGraphics
        PrintJob
        Rectangle
        RenderingHints
        ScrollPane
        Scrollbar
        Shape
        Stroke
        SystemColor
        TextArea
        TextComponent
        TextField
        TexturePaint
        TexturePaintContext
        Toolkit
        Transparency
        Window
        (color(
            CMMException
            ColorSpace
            ICC_ColorSpace
            ICC_Profile
            ICC_ProfileGray
            ICC_ProfileRGB
            ProfileDataException
            ))
        (datatransfer(
            Clipboard
            ClipboardOwner
            DataFlavor
            FlavorMap
            MimeType
            MimeTypeParameterList
            MimeTypeParseException
            StringSelection
            SystemFlavorMap
            Transferable
            UnsupportedFlavorException
            ))
        (dnd(
            Autoscroll
            DnDConstants
            DragGestureEvent
            DragGestureListener
            DragGestureRecognizer
            DragSource
            DragSourceContext
            DragSourceDragEvent
            DragSourceDropEvent
            DragSourceEvent
            DragSourceListener
            DropTarget
            DropTargetContext
            DropTargetDragEvent
            DropTargetDropEvent
            DropTargetEvent
            DropTargetListener
            InvalidDnDOperationException
            MouseDragGestureRecognizer
            (peer(
                DragSourceContextPeer
                DropTargetContextPeer
                DropTargetPeer
                ))
            ))
        (event(
            AWTEventListener
            ActionEvent
            ActionListener
            AdjustmentEvent
            AdjustmentListener
            ComponentAdapter
            ComponentEvent
            ComponentListener
            ContainerAdapter
            ContainerEvent
            ContainerListener
            FocusAdapter
            FocusEvent
            FocusListener
            InputEvent
            InputMethodEvent
            InputMethodListener
            InvocationEvent
            ItemEvent
            ItemListener
            KeyAdapter
            KeyEvent
            KeyListener
            MouseAdapter
            MouseEvent
            MouseListener
            MouseMotionAdapter
            MouseMotionListener
            NativeLibLoader
            PaintEvent
            TextEvent
            TextListener
            WindowAdapter
            WindowEvent
            WindowListener
            ))
        (font(
            FontRenderContext
            GlyphJustificationInfo
            GlyphMetrics
            GlyphVector
            GraphicAttribute
            ImageGraphicAttribute
            LineBreakMeasurer
            LineMetrics
            MultipleMaster
            OpenType
            ShapeGraphicAttribute
            TextAttribute
            TextHitInfo
            TextJustifier
            TextLayout
            TextLine
            TextMeasurer
            TransformAttribute
            ))
        (geom(
            AffineTransform
            Arc2D
            ArcIterator
            Area
            CubicCurve2D
            CubicIterator
            Dimension2D
            Ellipse2D
            EllipseIterator
            FlatteningPathIterator
            GeneralPath
            GeneralPathIterator
            IllegalPathStateException
            Line2D
            LineIterator
            NoninvertibleTransformException
            PathIterator
            Point2D
            QuadCurve2D
            QuadIterator
            RectIterator
            Rectangle2D
            RectangularShape
            RoundRectIterator
            RoundRectangle2D
            ))
        (im(
            InputContext
            InputMethodHighlight
            InputMethodRequests
            InputSubset
            ))
        (image(
            AffineTransformOp
            AreaAveragingScaleFilter
            BandCombineOp
            BandedSampleModel
            BufferedImage
            BufferedImageFilter
            BufferedImageOp
            ByteLookupTable
            ColorConvertOp
            ColorModel
            ComponentColorModel
            ComponentSampleModel
            ConvolveOp
            CropImageFilter
            DataBuffer
            DataBufferByte
            DataBufferInt
            DataBufferShort
            DataBufferUShort
            DirectColorModel
            FilteredImageSource
            ImageConsumer
            ImageFilter
            ImageObserver
            ImageProducer
            ImagingOpException
            IndexColorModel
            Kernel
            LookupOp
            LookupTable
            MemoryImageSource
            MultiPixelPackedSampleModel
            PackedColorModel
            PixelGrabber
            PixelInterleavedSampleModel
            RGBImageFilter
            Raster
            RasterFormatException
            RasterOp
            RenderedImage
            ReplicateScaleFilter
            RescaleOp
            SampleModel
            ShortLookupTable
            SinglePixelPackedSampleModel
            TileObserver
            WritableRaster
            WritableRenderedImage
            (renderable(
                ContextualRenderedImageFactory
                ParameterBlock
                RenderContext
                RenderableImage
                RenderableImageOp
                RenderableImageProducer
                RenderedImageFactory
                ))
            ))
        (peer(
            ButtonPeer
            CanvasPeer
            CheckboxMenuItemPeer
            CheckboxPeer
            ChoicePeer
            ComponentPeer
            ContainerPeer
            DialogPeer
            FileDialogPeer
            FontPeer
            FramePeer
            LabelPeer
            LightweightPeer
            ListPeer
            MenuBarPeer
            MenuComponentPeer
            MenuItemPeer
            MenuPeer
            PanelPeer
            PopupMenuPeer
            ScrollPanePeer
            ScrollbarPeer
            TextAreaPeer
            TextComponentPeer
            TextFieldPeer
            WindowPeer
            ))
        (print(
            Book
            PageFormat
            Pageable
            Paper
            Printable
            PrinterAbortException
            PrinterException
            PrinterGraphics
            PrinterIOException
            PrinterJob
            ))
        ))
    (beans(
        AppletInitializer
        BeanDescriptor
        BeanInfo
        Beans
        Customizer
        DesignMode
        EventSetDescriptor
        FeatureDescriptor
        IndexedPropertyDescriptor
        IntrospectionException
        Introspector
        MethodDescriptor
        ParameterDescriptor
        PropertyChangeEvent
        PropertyChangeListener
        PropertyChangeSupport
        PropertyDescriptor
        PropertyEditor
        PropertyEditorManager
        PropertyEditorSupport
        PropertyVetoException
        SimpleBeanInfo
        VetoableChangeListener
        VetoableChangeSupport
        Visibility
        (beancontext(
            BeanContext
            BeanContextChild
            BeanContextChildComponentProxy
            BeanContextChildSupport
            BeanContextContainerProxy
            BeanContextEvent
            BeanContextMembershipEvent
            BeanContextMembershipListener
            BeanContextProxy
            BeanContextServiceAvailableEvent
            BeanContextServiceProvider
            BeanContextServiceProviderBeanInfo
            BeanContextServiceRevokedEvent
            BeanContextServiceRevokedListener
            BeanContextServices
            BeanContextServicesListener
            BeanContextServicesSupport
            BeanContextSupport
            ))
        ))
    (io(
        BufferedInputStream
        BufferedOutputStream
        BufferedReader
        BufferedWriter
        ByteArrayInputStream
        ByteArrayOutputStream
        CharArrayReader
        CharArrayWriter
        CharConversionException
        DataInput
        DataInputStream
        DataOutput
        DataOutputStream
        EOFException
        Externalizable
        File
        FileDescriptor
        FileFilter
        FileInputStream
        FileNotFoundException
        FileOutputStream
        FilePermission
        FileReader
        FileSystem
        FileWriter
        FilenameFilter
        FilterInputStream
        FilterOutputStream
        FilterReader
        FilterWriter
        IOException
        InputStream
        InputStreamReader
        InterruptedIOException
        InvalidClassException
        InvalidObjectException
        LineNumberInputStream
        LineNumberReader
        NotActiveException
        NotSerializableException
        ObjectInput
        ObjectInputStream
        ObjectInputValidation
        ObjectOutput
        ObjectOutputStream
        ObjectStreamClass
        ObjectStreamConstants
        ObjectStreamException
        ObjectStreamField
        OptionalDataException
        OutputStream
        OutputStreamWriter
        PipedInputStream
        PipedOutputStream
        PipedReader
        PipedWriter
        PrintStream
        PrintWriter
        PushbackInputStream
        PushbackReader
        RandomAccessFile
        Reader
        SequenceInputStream
        Serializable
        SerializablePermission
        StreamCorruptedException
        StreamTokenizer
        StringBufferInputStream
        StringReader
        StringWriter
        SyncFailedException
        UTFDataFormatException
        UnsupportedEncodingException
        WriteAbortedException
        Writer
        ))
    (lang(
        AbstractMethodError
        ArithmeticException
        ArrayIndexOutOfBoundsException
        ArrayStoreException
        Boolean
        Byte
        Character
        Class
        ClassCastException
        ClassCircularityError
        ClassFormatError
        ClassLoader
        ClassNotFoundException
        CloneNotSupportedException
        Cloneable
        Comparable
        Compiler
        Double
        Error
        Exception
        ExceptionInInitializerError
        Float
        FloatingDecimal
        IllegalAccessError
        IllegalAccessException
        IllegalArgumentException
        IllegalMonitorStateException
        IllegalStateException
        IllegalThreadStateException
        IncompatibleClassChangeError
        IndexOutOfBoundsException
        InheritableThreadLocal
        InstantiationError
        InstantiationException
        Integer
        InternalError
        InterruptedException
        LinkageError
        Long
        Math
        NegativeArraySizeException
        NoClassDefFoundError
        NoSuchFieldError
        NoSuchFieldException
        NoSuchMethodError
        NoSuchMethodException
        NullPointerException
        Number
        NumberFormatException
        Object
        OutOfMemoryError
        Package
        Process
        Runnable
        Runtime
        RuntimeException
        RuntimePermission
        SecurityException
        SecurityManager
        Short
        StackOverflowError
        String
        StringBuffer
        StringIndexOutOfBoundsException
        System
        Thread
        ThreadDeath
        ThreadGroup
        ThreadLocal
        Throwable
        UnknownError
        UnsatisfiedLinkError
        UnsupportedClassVersionError
        UnsupportedOperationException
        VerifyError
        VirtualMachineError
        Void
        (ref(
            FinalReference
            Finalizer
            PhantomReference
            Reference
            ReferenceQueue
            SoftReference
            WeakReference
            ))
        (reflect(
            AccessibleObject
            Array
            Constructor
            Field
            InvocationTargetException
            Member
            Method
            Modifier
            ReflectPermission
            ))
        ))
    (math(
        BigDecimal
        BigInteger
        ))
    (net(
        Authenticator
        BindException
        ConnectException
        ContentHandler
        ContentHandlerFactory
        DatagramPacket
        DatagramSocket
        DatagramSocketImpl
        FileNameMap
        HttpURLConnection
        InetAddress
        JarURLConnection
        MalformedURLException
        MulticastSocket
        NetPermission
        NoRouteToHostException
        PasswordAuthentication
        PlainDatagramSocketImpl
        PlainSocketImpl
        ProtocolException
        ServerSocket
        Socket
        SocketException
        SocketImpl
        SocketImplFactory
        SocketInputStream
        SocketOptions
        SocketOutputStream
        SocketPermission
        URL
        URLClassLoader
        URLConnection
        URLDecoder
        URLEncoder
        URLStreamHandler
        URLStreamHandlerFactory
        UnknownHostException
        UnknownServiceException
        ))
    (rmi(
        AccessException
        AlreadyBoundException
        ConnectException
        ConnectIOException
        MarshalException
        MarshalledObject
        Naming
        NoSuchObjectException
        NotBoundException
        RMISecurityException
        RMISecurityManager
        Remote
        RemoteException
        ServerError
        ServerException
        ServerRuntimeException
        StubNotFoundException
        UnexpectedException
        UnknownHostException
        UnmarshalException
        (activation(
            Activatable
            ActivateFailedException
            ActivationDesc
            ActivationException
            ActivationGroup
            ActivationGroupDesc
            ActivationGroupID
            ActivationID
            ActivationInstantiator
            ActivationMonitor
            ActivationSystem
            Activator
            UnknownGroupException
            UnknownObjectException
            ))
        (dgc(
            DGC
            Lease
            VMID
            ))
        (registry(
            LocateRegistry
            Registry
            RegistryHandler
            ))
        (server(
            ExportException
            LoaderHandler
            LogStream
            ObjID
            Operation
            RMIClassLoader
            RMIClientSocketFactory
            RMIFailureHandler
            RMIServerSocketFactory
            RMISocketFactory
            RemoteCall
            RemoteObject
            RemoteRef
            RemoteServer
            RemoteStub
            ServerCloneException
            ServerNotActiveException
            ServerRef
            Skeleton
            SkeletonMismatchException
            SkeletonNotFoundException
            SocketSecurityException
            UID
            UnicastRemoteObject
            Unreferenced
            ))
        ))
    (security(
        AccessControlContext
        AccessControlException
        AccessController
        AlgorithmParameterGenerator
        AlgorithmParameterGeneratorSpi
        AlgorithmParameters
        AlgorithmParametersSpi
        AllPermission
        BasicPermission
        Certificate
        CodeSource
        DigestException
        DigestInputStream
        DigestOutputStream
        GeneralSecurityException
        Guard
        GuardedObject
        Identity
        IdentityScope
        InvalidAlgorithmParameterException
        InvalidKeyException
        InvalidParameterException
        Key
        KeyException
        KeyFactory
        KeyFactorySpi
        KeyManagementException
        KeyPair
        KeyPairGenerator
        KeyPairGeneratorSpi
        KeyStore
        KeyStoreException
        KeyStoreSpi
        MessageDigest
        MessageDigestSpi
        NoSuchAlgorithmException
        NoSuchProviderException
        Permission
        PermissionCollection
        Permissions
        Policy
        Principal
        PrivateKey
        PrivilegedAction
        PrivilegedActionException
        PrivilegedExceptionAction
        ProtectionDomain
        Provider
        ProviderException
        PublicKey
        SecureClassLoader
        SecureRandom
        SecureRandomSpi
        Security
        SecurityPermission
        Signature
        SignatureException
        SignatureSpi
        SignedObject
        Signer
        UnrecoverableKeyException
        UnresolvedPermission
        UnresolvedPermissionCollection
        (acl(
            Acl
            AclEntry
            AclNotFoundException
            Group
            LastOwnerException
            NotOwnerException
            Owner
            Permission
            ))
        (cert(
            CRL
            CRLException
            Certificate
            CertificateEncodingException
            CertificateException
            CertificateExpiredException
            CertificateFactory
            CertificateFactorySpi
            CertificateNotYetValidException
            CertificateParsingException
            X509CRL
            X509CRLEntry
            X509Certificate
            X509Extension
            ))
        (interfaces(
            DSAKey
            DSAKeyPairGenerator
            DSAParams
            DSAPrivateKey
            DSAPublicKey
            RSAPrivateCrtKey
            RSAPrivateKey
            RSAPublicKey
            ))
        (spec(
            AlgorithmParameterSpec
            DSAParameterSpec
            DSAPrivateKeySpec
            DSAPublicKeySpec
            EncodedKeySpec
            InvalidKeySpecException
            InvalidParameterSpecException
            KeySpec
            PKCS8EncodedKeySpec
            RSAPrivateCrtKeySpec
            RSAPrivateKeySpec
            RSAPublicKeySpec
            X509EncodedKeySpec
            ))
        ))
    (sql(
        Array
        BatchUpdateException
        Blob
        CallableStatement
        Clob
        Connection
        DataTruncation
        DatabaseMetaData
        Date
        Driver
        DriverManager
        DriverPropertyInfo
        PreparedStatement
        Ref
        ResultSet
        ResultSetMetaData
        SQLData
        SQLException
        SQLInput
        SQLOutput
        SQLWarning
        Statement
        Struct
        Time
        Timestamp
        Types
        ))
    (text(
        Annotation
        AttributedCharacterIterator
        AttributedString
        BreakIterator
        CharacterBreakData
        CharacterIterator
        ChoiceFormat
        CollationElementIterator
        CollationKey
        CollationRules
        Collator
        CompactByteArray
        CompactCharArray
        CompactIntArray
        CompactShortArray
        CompactStringArray
        DateFormat
        DateFormatSymbols
        DecimalFormat
        DecimalFormatSymbols
        DigitList
        EntryPair
        FieldPosition
        Format
        IntHashtable
        LineBreakData
        MergeCollation
        MessageFormat
        Normalizer
        NumberFormat
        ParseException
        ParsePosition
        PatternEntry
        RuleBasedCollator
        SentenceBreakData
        SimpleDateFormat
        SimpleTextBoundary
        SpecialMapping
        StringCharacterIterator
        TextBoundaryData
        UnicodeClassMapping
        Utility
        WordBreakData
        WordBreakTable
        (resources(
            DateFormatZoneData
            DateFormatZoneData_en
            LocaleData
            LocaleElements
            LocaleElements_en
            LocaleElements_en_US
            ))
        ))
    (util(
        AbstractCollection
        AbstractList
        AbstractMap
        AbstractSequentialList
        AbstractSet
        ArrayList
        Arrays
        BitSet
        Calendar
        Collection
        Collections
        Comparator
        ConcurrentModificationException
        Date
        Dictionary
        EmptyStackException
        Enumeration
        EventListener
        EventObject
        GregorianCalendar
        HashMap
        HashSet
        Hashtable
        Iterator
        LinkedList
        List
        ListIterator
        ListResourceBundle
        Locale
        Map
        MissingResourceException
        NoSuchElementException
        Observable
        Observer
        Properties
        PropertyPermission
        PropertyResourceBundle
        Random
        ResourceBundle
        Set
        SimpleTimeZone
        SortedMap
        SortedSet
        Stack
        StringTokenizer
        TimeZone
        TooManyListenersException
        TreeMap
        TreeSet
        Vector
        WeakHashMap
        (jar(
            Attributes
            JarEntry
            JarException
            JarFile
            JarInputStream
            JarOutputStream
            JarVerifier
            Manifest
            ))
        (zip(
            Adler32
            CRC32
            CheckedInputStream
            CheckedOutputStream
            Checksum
            DataFormatException
            Deflater
            DeflaterOutputStream
            GZIPInputStream
            GZIPOutputStream
            Inflater
            InflaterInputStream
            ZipConstants
            ZipEntry
            ZipException
            ZipFile
            ZipInputStream
            ZipOutputStream
            ))
        ))
    ))
(javax(
    (accessibility(
        Accessible
        AccessibleAction
        AccessibleBundle
        AccessibleComponent
        AccessibleContext
        AccessibleHyperlink
        AccessibleHypertext
        AccessibleResourceBundle
        AccessibleRole
        AccessibleSelection
        AccessibleState
        AccessibleStateSet
        AccessibleText
        AccessibleValue
        ))
    (swing(
        AbstractAction
        AbstractButton
        AbstractListModel
        Action
        AncestorNotifier
        AppContext
        Autoscroller
        BorderFactory
        BoundedRangeModel
        Box
        BoxLayout
        ButtonGroup
        ButtonModel
        CellEditor
        CellRendererPane
        ComboBoxEditor
        ComboBoxModel
        DebugGraphics
        DebugGraphicsFilter
        DebugGraphicsInfo
        DebugGraphicsObserver
        DefaultBoundedRangeModel
        DefaultButtonModel
        DefaultCellEditor
        DefaultComboBoxModel
        DefaultDesktopManager
        DefaultFocusManager
        DefaultListCellRenderer
        DefaultListModel
        DefaultListSelectionModel
        DefaultSingleSelectionModel
        DesktopManager
        FocusManager
        GraphicsWrapper
        GrayFilter
        Icon
        ImageIcon
        JApplet
        JButton
        JCheckBox
        JCheckBoxMenuItem
        JColorChooser
        JComboBox
        JComponent
        JDesktopPane
        JDialog
        JEditorPane
        JFileChooser
        JFrame
        JInternalFrame
        JLabel
        JLayeredPane
        JList
        JMenu
        JMenuBar
        JMenuItem
        JOptionPane
        JPanel
        JPasswordField
        JPopupMenu
        JProgressBar
        JRadioButton
        JRadioButtonMenuItem
        JRootPane
        JScrollBar
        JScrollPane
        JSeparator
        JSlider
        JSplitPane
        JTabbedPane
        JTable
        JTextArea
        JTextField
        JTextPane
        JToggleButton
        JToolBar
        JToolTip
        JTree
        JViewport
        JWindow
        KeyStroke
        KeyboardManager
        ListCellRenderer
        ListModel
        ListSelectionModel
        LookAndFeel
        MenuElement
        MenuSelectionManager
        MultiUIDefaults
        MutableComboBoxModel
        OverlayLayout
        ProgressMonitor
        ProgressMonitorInputStream
        Renderer
        RepaintManager
        RootPaneContainer
        ScrollPaneConstants
        ScrollPaneLayout
        Scrollable
        SingleSelectionModel
        SizeRequirements
        SwingConstants
        SwingGraphics
        SwingUtilities
        SystemEventQueueUtilities
        Timer
        TimerQueue
        ToolTipManager
        UIDefaults
        UIManager
        UnsupportedLookAndFeelException
        ViewportLayout
        WindowConstants
        (border(
            AbstractBorder
            BevelBorder
            Border
            CompoundBorder
            EmptyBorder
            EtchedBorder
            LineBorder
            MatteBorder
            SoftBevelBorder
            TitledBorder
            ))
        (colorchooser(
            AbstractColorChooserPanel
            CenterLayout
            ColorChooserComponentFactory
            ColorSelectionModel
            DefaultColorSelectionModel
            DefaultHSBChooserPanel
            DefaultPreviewPanel
            DefaultRGBChooserPanel
            DefaultSwatchChooserPanel
            JIntegerTextField
            SmartGridLayout
            SyntheticImage
            ))
        (event(
            AncestorEvent
            AncestorListener
            CaretEvent
            CaretListener
            CellEditorListener
            ChangeEvent
            ChangeListener
            DocumentEvent
            DocumentListener
            EventListenerList
            HyperlinkEvent
            HyperlinkListener
            InternalFrameAdapter
            InternalFrameEvent
            InternalFrameListener
            ListDataEvent
            ListDataListener
            ListSelectionEvent
            ListSelectionListener
            MenuDragMouseEvent
            MenuDragMouseListener
            MenuEvent
            MenuKeyEvent
            MenuKeyListener
            MenuListener
            MouseInputAdapter
            MouseInputListener
            PopupMenuEvent
            PopupMenuListener
            SwingPropertyChangeSupport
            TableColumnModelEvent
            TableColumnModelListener
            TableModelEvent
            TableModelListener
            TreeExpansionEvent
            TreeExpansionListener
            TreeModelEvent
            TreeModelListener
            TreeSelectionEvent
            TreeSelectionListener
            TreeWillExpandListener
            UndoableEditEvent
            UndoableEditListener
            ))
        (filechooser(
            FileFilter
            FileSystemView
            FileView
            ))
        (plaf(
            BorderUIResource
            ButtonUI
            ColorChooserUI
            ColorUIResource
            ComboBoxUI
            ComponentUI
            DesktopIconUI
            DesktopPaneUI
            DimensionUIResource
            FileChooserUI
            FontUIResource
            IconUIResource
            InsetsUIResource
            InternalFrameUI
            LabelUI
            ListUI
            MenuBarUI
            MenuItemUI
            OptionPaneUI
            PanelUI
            PopupMenuUI
            ProgressBarUI
            ScrollBarUI
            ScrollPaneUI
            SeparatorUI
            SliderUI
            SplitPaneUI
            TabbedPaneUI
            TableHeaderUI
            TableUI
            TextUI
            ToolBarUI
            ToolTipUI
            TreeUI
            UIResource
            ViewportUI
            (basic(
                BasicArrowButton
                BasicBorders
                BasicButtonListener
                BasicButtonUI
                BasicCheckBoxMenuItemUI
                BasicCheckBoxUI
                BasicColorChooserUI
                BasicComboBoxEditor
                BasicComboBoxRenderer
                BasicComboBoxUI
                BasicComboPopup
                BasicDesktopIconUI
                BasicDesktopPaneUI
                BasicDirectoryModel
                BasicEditorPaneUI
                BasicFileChooserUI
                BasicGraphicsUtils
                BasicHTML
                BasicIconFactory
                BasicInternalFrameTitlePane
                BasicInternalFrameUI
                BasicLabelUI
                BasicListUI
                BasicLookAndFeel
                BasicMenuBarUI
                BasicMenuItemUI
                BasicMenuUI
                BasicOptionPaneUI
                BasicPanelUI
                BasicPasswordFieldUI
                BasicPopupMenuSeparatorUI
                BasicPopupMenuUI
                BasicProgressBarUI
                BasicRadioButtonMenuItemUI
                BasicRadioButtonUI
                BasicScrollBarUI
                BasicScrollPaneUI
                BasicSeparatorUI
                BasicSliderUI
                BasicSplitPaneDivider
                BasicSplitPaneUI
                BasicTabbedPaneUI
                BasicTableHeaderUI
                BasicTableUI
                BasicTextAreaUI
                BasicTextFieldUI
                BasicTextPaneUI
                BasicTextUI
                BasicToggleButtonUI
                BasicToolBarSeparatorUI
                BasicToolBarUI
                BasicToolTipUI
                BasicTreeUI
                BasicViewportUI
                CenterLayout
                ComboPopup
                DefaultMenuLayout
                ))
            (metal(
                DefaultMetalTheme
                MetalBorders
                MetalBumps
                MetalButtonUI
                MetalCheckBoxIcon
                MetalCheckBoxUI
                MetalComboBoxButton
                MetalComboBoxEditor
                MetalComboBoxIcon
                MetalComboBoxUI
                MetalDesktopIconUI
                MetalFileChooserUI
                MetalIconFactory
                MetalInternalFrameTitlePane
                MetalInternalFrameUI
                MetalLabelUI
                MetalLookAndFeel
                MetalPopupMenuSeparatorUI
                MetalProgressBarUI
                MetalRadioButtonUI
                MetalScrollBarUI
                MetalScrollButton
                MetalScrollPaneUI
                MetalSeparatorUI
                MetalSliderUI
                MetalSplitPaneDivider
                MetalSplitPaneUI
                MetalTabbedPaneUI
                MetalTextFieldUI
                MetalTheme
                MetalToggleButtonUI
                MetalToolBarUI
                MetalToolTipUI
                MetalTreeUI
                MetalUtils
                ))
            (multi(
                MultiButtonUI
                MultiColorChooserUI
                MultiComboBoxUI
                MultiDesktopIconUI
                MultiDesktopPaneUI
                MultiFileChooserUI
                MultiInternalFrameUI
                MultiLabelUI
                MultiListUI
                MultiLookAndFeel
                MultiMenuBarUI
                MultiMenuItemUI
                MultiOptionPaneUI
                MultiPanelUI
                MultiPopupMenuUI
                MultiProgressBarUI
                MultiScrollBarUI
                MultiScrollPaneUI
                MultiSeparatorUI
                MultiSliderUI
                MultiSplitPaneUI
                MultiTabbedPaneUI
                MultiTableHeaderUI
                MultiTableUI
                MultiTextUI
                MultiToolBarUI
                MultiToolTipUI
                MultiTreeUI
                MultiViewportUI
                ))
            ))
        (table(
            AbstractTableModel
            DefaultTableCellRenderer
            DefaultTableColumnModel
            DefaultTableModel
            JTableHeader
            TableCellEditor
            TableCellRenderer
            TableColumn
            TableColumnModel
            TableModel
            ))
        (text(
            AbstractDocument
            AbstractWriter
            ArabicLigaturizer
            AttributeSet
            BadLocationException
            Bidi
            BigBoxView
            BoxView
            Caret
            ChangedCharSetException
            CharBasedLigaturizer
            ComponentView
            CompositeView
            DefaultCaret
            DefaultEditorKit
            DefaultHighlighter
            DefaultStyledDocument
            DefaultTextUI
            Document
            EditorKit
            Element
            ElementIterator
            ExtendedTextLabel
            FieldView
            GapContent
            GapVector
            Highlighter
            IconView
            JTextComponent
            Keymap
            LabelView
            LayeredHighlighter
            Ligaturizer
            MutableAttributeSet
            NewArabicShaping
            ParagraphView
            PasswordView
            PlainDocument
            PlainView
            Position
            RLEUtilities
            Segment
            SimpleAttributeSet
            StandardExtendedTextLabel
            StateInvariantError
            StringContent
            Style
            StyleConstants
            StyleContext
            StyledDocument
            StyledEditorKit
            TabExpander
            TabSet
            TabStop
            TabableView
            TableView
            TextAction
            TextLabel
            Utilities
            View
            ViewFactory
            WrappedPlainView
            (html(
                BRView
                BlockView
                CSS
                CommentView
                EditableView
                FormView
                FrameSetView
                FrameView
                HRuleView
                HTML
                HTMLDocument
                HTMLEditorKit
                HTMLFrameHyperlinkEvent
                HTMLWriter
                HiddenTagView
                ImageView
                InlineView
                IsindexView
                LineView
                ListView
                Map
                MinimalHTMLWriter
                NoFramesView
                ObjectView
                Option
                OptionComboBoxModel
                OptionListModel
                ParagraphView
                ResourceLoader
                StyleSheet
                TableView
                TextAreaDocument
                (parser(
                    AttributeList
                    ContentModel
                    ContentModelState
                    DTD
                    DTDConstants
                    DocumentParser
                    Element
                    Entity
                    Parser
                    ParserDelegator
                    ResourceLoader
                    TagElement
                    TagStack
                    ))
                ))
            (rtf(
                AbstractFilter
                Constants
                MockAttributeSet
                RTFAttribute
                RTFAttributes
                RTFEditorKit
                RTFGenerator
                RTFParser
                RTFReader
                ))
            ))
        (tree(
            AbstractLayoutCache
            DefaultMutableTreeNode
            DefaultTreeCellEditor
            DefaultTreeCellRenderer
            DefaultTreeModel
            DefaultTreeSelectionModel
            ExpandVetoException
            FixedHeightLayoutCache
            MutableTreeNode
            RowMapper
            TreeCellEditor
            TreeCellRenderer
            TreeModel
            TreeNode
            TreePath
            TreeSelectionModel
            VariableHeightLayoutCache
            ))
        (undo(
            AbstractUndoableEdit
            CannotRedoException
            CannotUndoException
            CompoundEdit
            StateEdit
            StateEditable
            UndoManager
            UndoableEdit
            UndoableEditSupport
            ))
        ))
    ))
(org(
    (omg(
        (CORBA(
            ARG_IN
            ARG_INOUT
            ARG_OUT
            Any
            AnyHolder
            BAD_CONTEXT
            BAD_INV_ORDER
            BAD_OPERATION
            BAD_PARAM
            BAD_POLICY
            BAD_POLICY_TYPE
            BAD_POLICY_VALUE
            BAD_TYPECODE
            BooleanHolder
            Bounds
            ByteHolder
            COMM_FAILURE
            CTX_RESTRICT_SCOPE
            CharHolder
            CompletionStatus
            Context
            ContextList
            Current
            DATA_CONVERSION
            DefinitionKind
            DomainManager
            DoubleHolder
            DynAny
            (DynAnyPackage(
                Invalid
                InvalidSeq
                InvalidValue
                TypeMismatch
                ))
            DynArray
            DynEnum
            DynFixed
            DynSequence
            DynStruct
            DynUnion
            DynValue
            DynamicImplementation
            Environment
            ExceptionList
            FREE_MEM
            FixedHolder
            FloatHolder
            IDLType
            IMP_LIMIT
            INITIALIZE
            INTERNAL
            INTF_REPOS
            INVALID_TRANSACTION
            INV_FLAG
            INV_IDENT
            INV_OBJREF
            INV_POLICY
            IRObject
            IntHolder
            LongHolder
            MARSHAL
            NO_IMPLEMENT
            NO_MEMORY
            NO_PERMISSION
            NO_RESOURCES
            NO_RESPONSE
            NVList
            NameValuePair
            NamedValue
            OBJECT_NOT_EXIST
            OBJ_ADAPTER
            ORB
            (ORBPackage(
                InconsistentTypeCode
                InvalidName
                ))
            Object
            ObjectHolder
            PERSIST_STORE
            PRIVATE_MEMBER
            PUBLIC_MEMBER
            Policy
            PolicyError
            Principal
            PrincipalHolder
            Request
            ServerRequest
            ServiceDetail
            ServiceDetailHelper
            ServiceInformation
            ServiceInformationHelper
            ServiceInformationHolder
            SetOverrideType
            ShortHolder
            StringHolder
            StructMember
            SystemException
            TCKind
            TRANSACTION_REQUIRED
            TRANSACTION_ROLLEDBACK
            TRANSIENT
            TypeCode
            TypeCodeHolder
            (TypeCodePackage(
                BadKind
                Bounds
                ))
            UNKNOWN
            UNSUPPORTED_POLICY
            UNSUPPORTED_POLICY_VALUE
            UnionMember
            UnknownUserException
            UserException
            VM_ABSTRACT
            VM_CUSTOM
            VM_NONE
            VM_TRUNCATABLE
            ValueMember
            WrongTransaction
            (portable(
                ApplicationException
                Delegate
                IDLEntity
                InputStream
                InvokeHandler
                ObjectImpl
                OutputStream
                RemarshalException
                ResponseHandler
                ServantObject
                Streamable
                ))
            ))
        (CosNaming(
            Binding
            BindingHelper
            BindingHolder
            BindingIterator
            BindingIteratorHelper
            BindingIteratorHolder
            BindingListHelper
            BindingListHolder
            BindingType
            BindingTypeHelper
            BindingTypeHolder
            IstringHelper
            NameComponent
            NameComponentHelper
            NameComponentHolder
            NameHelper
            NameHolder
            NamingContext
            NamingContextHelper
            NamingContextHolder
            (NamingContextPackage(
                AlreadyBound
                AlreadyBoundHelper
                AlreadyBoundHolder
                CannotProceed
                CannotProceedHelper
                CannotProceedHolder
                InvalidName
                InvalidNameHelper
                InvalidNameHolder
                NotEmpty
                NotEmptyHelper
                NotEmptyHolder
                NotFound
                NotFoundHelper
                NotFoundHolder
                NotFoundReason
                NotFoundReasonHelper
                NotFoundReasonHolder
                ))
            _BindingIteratorImplBase
            _BindingIteratorStub
            _NamingContextImplBase
            _NamingContextStub
            ))
        ))
    ))
(sun(
    (tools(
        (ttydebug(
            TTY
            ))
        ))
    ))
(sunw(
    (io(
        Serializable
        ))
    (util(
        EventListener
        EventObject
        ))
    ))
))


(define root (DefaultMutableTreeNode. ""))
(define treemodel (DefaultTreeModel. root))

(define (tree->node x)
  (if (.isInstance Pair.class x)
      (let ((N (DefaultMutableTreeNode. (.first$ x))))
        (begin
          (for-each (lambda (y) (insertNode y N)) (.second x))
          N))
      (DefaultMutableTreeNode. x))
)

(define (insertNode y parent)
  (.insertNodeInto treemodel (tree->node y) parent (.getChildCount treemodel parent)))

(for-each (lambda(y) (insertNode y root)) classtree)
         



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define win (JFrame. "Class Browser"))

(define desktop (JDesktopPane.))
(.add (.getContentPane win) desktop BorderLayout.CENTER$)
(.resize win 500 500)

(define P (JInternalFrame. "Class Selector" #t #f #t #t))
(.add desktop P 1)
(define T (JTextField. "java.lang.Object" 30))
(.setFont T (Font. "TimesRoman" 1 18))
(.add (.getContentPane P) T BorderLayout.SOUTH$)



;  (define tree (JTree. classtable))
(define tree (JTree. treemodel))
(.add (.getContentPane P) (JScrollPane. tree) BorderLayout.CENTER$)

(.setSize P 200 400)
(.show P)


(define (bool V) (if (.equals V #f) #f #t))

(define blanks "                                                                                             ")

(define (printit out x )
  (.print out "  ")
  (.println out (.getName x))
  (.print out "\t")
  (.print out    x)
  (.print out "\n")
)

(define (myToString x)
  (if (eq? x #null) "null" 
      (.toString x)))


(define (filterPublic L)
  (if (.equals L ()) ()
     (if (.equals #t (Modifier.isPublic (.getModifiers (.first$ L))))
         (cons (.first$ L) (filterPublic (.rest$ L)))
         (filterPublic (.rest$ L)))))


(define (class-describe Class What)
  (define sout (StringWriter.))
  (define out (PrintWriter. sout))
  (define (printitem x) (printit out x))

  (if (or (bool (.equals "all" What)) (bool (.equals "constructors" What)))
  (begin
    (.println out "\nCONSTRUCTORS\n")
    (for-each printitem (filterPublic (array->list (.getConstructors Class)))  )))

  (if (or (bool (.equals "all" What)) (bool (.equals "fields" What)))
  (begin
    (.println out  "\nFIELDS\n")
    (for-each printitem  (filterPublic (array->list (.getFields Class))))))

  (if (or (bool (.equals "all" What)) (bool (.equals "methods" What)))
  (begin
    (.println out  "\nMETHODS\n")
    (for-each printitem (filterPublic (array->list (.getMethods Class))))))

  (if (or (bool (.equals "all" What)) (bool (.equals "classes" What)))
  (begin
    (.println out "\nMEMBER CLASSES\n")
    (for-each printitem (array->list (.getClasses Class)))))

  (if (or (bool (.equals "all" What)) (bool (.equals "interfaces" What)))
  (begin
    (.println out  "\nINTERFACES\n")
    (for-each printitem (array->list (.getInterfaces Class)))))

  (.println out (.concat "\n\nCLASS MODIFIERS: " (Modifier.toString (.getModifiers Class))))
;  (.println out (.concat "PACKAGE: " (myToString (.getPackage Class))))
  (.println out (.concat "DECLARING CLASS: " (myToString (.getDeclaringClass Class))))
  (.println out (.concat "SUPERCLASS: " (myToString(.getSuperclass Class))))
  (.println out (.concat "ISARRAY: " (myToString (.isArray Class))))

  (.close out)
  (.toString sout)
)






(define makeClassSummary   
   (lambda (e)
    (begin
       (define newwin (JInternalFrame. (.getText T) #t #t #t #t))
       (define TA
             (JTextArea.
              (tryCatch
                (class-describe (Class.forName (.getText T)) "all")
                (lambda(e) (string-append "Class " (.getText T) " not found")))))
       (.add desktop newwin 1)
       (.add (.getContentPane newwin)
          (JScrollPane. TA))
       (.setCaretPosition TA 0)
       (.setSize newwin 400 200)
;       (.setMaximum newwin #t)
       (.show newwin))))

(.addActionListener T
  (Listener11. makeClassSummary))

(define (pathToString L)
  (if (= (.length L) 1) (.toString (.first$ L))
      (.concat (.toString (.first$ L))
         (.concat "." (pathToString (.rest$ L)))))) 



(.addTreeSelectionListener tree (Listener. (lambda (e)
  (let ((selection (pathToString (vector->list (.getPath (.getSelectionPath tree))))))
    (begin
      (.println System.out$ (.getSelectionPath tree))
      (.setText T  (.substring selection 1))
      (makeClassSummary e)
  )))))
   

(.hide progress-win)
(.show win)


(define classtable (Hashtable.))

(define (main ShellArgs) #t)

(define (uncracked-class-list class-list)
  (map (lambda (cracked-class)
	 (tryCatch
	  (Class.forName (apply string-append (separate "." cracked-class)))
	  (lambda (e) (display (string-append "can't complete " cracked-class
					      "\n"))
		  #f)))
       class-list))

'(begin (set! cs (uncracked-class-list classlist)) (length cs))
