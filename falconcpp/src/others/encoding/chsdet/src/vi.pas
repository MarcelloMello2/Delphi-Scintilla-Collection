unit vi;

interface

const
  FILE_VERSION_MAJOR	     =  	0;
  FILE_VERSION_MINOR	     =  	2;
  FILE_VERSION_RELEASE	   =   	8;
  FILE_VERSION_BUILD	     =  	4;

  // XXX_STR is ASCII code of XXX (if it less than 10)
  FILE_VERSION_MAJOR_STR	 		=	$30 + FILE_VERSION_MAJOR;
  FILE_VERSION_MINOR_STR   		=	$30 + FILE_VERSION_MINOR;
  FILE_VERSION_RELEASE_STR 		=	$30 + FILE_VERSION_RELEASE;
  FILE_VERSION_BUILD_STR  		=	$30 + FILE_VERSION_BUILD;

  PRODUCT_VERSION_MAJOR	   =   FILE_VERSION_MAJOR;
  PRODUCT_VERSION_MINOR	   =   FILE_VERSION_MINOR;
  PRODUCT_VERSION_RELEASE  =   FILE_VERSION_RELEASE;
  PRODUCT_VERSION_BUILD    =   FILE_VERSION_BUILD;

  PRODUCT_VERSION_MAJOR_STR  	=	FILE_VERSION_MAJOR_STR	;
  PRODUCT_VERSION_MINOR_STR  	=	FILE_VERSION_MINOR_STR  ;
  PRODUCT_VERSION_RELEASE_STR	=	FILE_VERSION_RELEASE_STR;
  PRODUCT_VERSION_BUILD_STR  	=	FILE_VERSION_BUILD_STR  ;


implementation

end.
