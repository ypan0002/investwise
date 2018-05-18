<form id="tb_library_item_form">
	<div id="themify_builder_lightbox_options_tab_items">
	<?php $form_title =  $type=='module' ? 'Save Module' : 'Save Row'; ?>
		<li class="title"><?php _e($form_title, 'themify'); ?></li>
	</div>

	<div id="themify_builder_lightbox_actions_items">
		<button id="builder_submit_library_item_form" class="builder_button"><?php _e('Save', 'themify') ?></button>
	</div>

	<div id="themify_builder_layout_part_form" class="themify_builder_options_tab_wrapper">
		<div class="themify_builder_options_tab_content">
			<?php themify_builder_module_settings_field( $fields ); ?>
		</div>
		<input type="hidden" name="postid" value="<?php echo esc_attr( $postid ); ?>">
		<input type="hidden" name="item" value="<?php echo esc_attr( base64_encode(($item)) ); ?>">
		<input type="hidden" name="type" value="<?php echo esc_attr( $type ); ?>">
		<input type="hidden" name="model" value="<?php echo esc_attr( $model ); ?>">
	</div>
</form>