<?php
if (!defined('ABSPATH'))
	exit; // Exit if accessed directly

global $ThemifyBuilder, $post;
if ( is_object( $post ) ) {
	$saved_post = clone $post;
}

$post = get_post( $builder_id );
$styles = $ThemifyBuilder->stylesheet->test_and_enqueue( true );
if ( $styles ) {
	$fonts = $ThemifyBuilder->stylesheet->enqueue_fonts( array() );
?>
	<link type="text/css" rel="stylesheet" href="<?php echo $styles['url']?>" />
	<?php if ( ! empty( $fonts ) ) : ?>
		<link type="text/css" rel="stylesheet" href="//fonts.googleapis.com/css?family=<?php echo implode( '|', $fonts ); ?>" />
	<?php endif;?>
<?php
}

if ( isset( $saved_post ) && is_object( $saved_post ) ) {
	$post = $saved_post;
}
?>
<style>
	.themify_builder.not_editable_builder .module_column {
		outline: initial !important;
	}
	.themify_builder_active:not(.tb-preview-only) .themify_builder:not(.not_editable_builder) .module_subrow {
		margin:0px;
	}
</style>
<div class="themify_builder_content themify_builder_content-<?php echo $builder_id; ?> themify_builder not_editable_builder">
	<?php
	foreach ($builder_output as $rows => $row) :
		if (!empty($row)) {
			echo Themify_Builder_Component_Row::template($rows, $row, $builder_id, false, false);
		}
	endforeach; // end row loop
	?>

</div>