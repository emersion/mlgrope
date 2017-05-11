open Graphics

type image_source =
	| Ppm of in_channel
	| Ppm_file of string
	| Rotation of float * image_source
	| Mirror of image_source

val load : image_source -> image
val get : image_source -> (unit -> image)
