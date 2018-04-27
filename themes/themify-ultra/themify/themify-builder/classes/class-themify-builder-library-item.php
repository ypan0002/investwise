<?php
/**
 * This file defines Builder Library Items designs and parts.
 *
 * Themify_Builder_Row class register post type for Library Items designs and Parts
 * Custom metabox, and load Library Items designs / parts.
 * 
 *
 * @package    Themify_Builder
 * @subpackage Themify_Builder/classes
 */

/**
 * The Builder Library Items class.
 *
 * This class register post type for Library Items designs and Parts
 * Custom metabox, and load Library Items designs / parts
 *
 *
 * @package    Themify_Builder
 * @subpackage Themify_Builder/classes
 * @author     Themify
 */
class Themify_Builder_Library_Items {
	
	public $post_type_name = array('row'=>'library_rows', 'module' => 'library_modules', 'part' => 'tbuilder_layout_part');
	private $user = 0;

	/**
	 * Constructor
	 * 
	 * @access public
	 */
	public function __construct($builder) {
		if($builder->stylesheet)
			$this->stylesheet = $builder->stylesheet;
		else 
			$this->stylesheet = false;
		
		if(!function_exists('themify_render_styling_settings'))
			include_once THEMIFY_BUILDER_INCLUDES_DIR . '/themify-builder-options.php';
			
		// Ajax Hooks
		add_action( 'wp_ajax_tb_library_item_form', array( $this, 'custom_library_item_form_ajaxify' ) );
		add_action( 'wp_ajax_tb_save_custom_item', array( $this, 'save_custom_item_ajaxify' ) );
		add_action( 'wp_ajax_tb_get_library_items', array( $this, 'list_library_items_ajax' ), 10, 2 );
		add_action( 'wp_ajax_tb_remove_library_item', array( $this, 'remove_library_item_ajax' ) );
		
		$this->user = get_current_user_id();
		
	}

	/**
	 * Render Row Form in lightbox
	 * 
	 * @access public
	 */
	public function custom_library_item_form_ajaxify() {
		check_ajax_referer( 'tb_load_nonce', 'nonce' );
		$postid = (int) $_POST['postid'];
		$item = $_POST['item'];
		$type = $_POST['type'];
		$model = $_POST['model'];

		$fields = array(
			array(
				'id' => 'item_title_field',
				'type' => 'text',
				'label' => __('Title', 'themify')
			),
			array(
				'id' => 'item_layout_save',
				'type' => 'checkbox',
				'label' => '',
				'options' => array(
					array( 'name' => 'layout_part', 'value' => sprintf('%s (<a href="https://themify.me/docs/builder#layout-parts" target="_blank">?</a>)', __('Save as Layout Part', 'themify') ))
				)
			)
		);
		
		include_once THEMIFY_BUILDER_INCLUDES_DIR . '/themify-builder-library-item-form.php';
		die();
	}

	/**
	 * Save as Row
	 * 
	 * @access public
	 */
	public function save_custom_item_ajaxify() {

		check_ajax_referer( 'tb_load_nonce', 'nonce' );

		$data = array();
		$response = array(
			'status' => 'failed',
			'msg' => __('Something went wrong', 'themify')
		);

		if ( isset( $_POST['form_data'] ) )
			parse_str( $_POST['form_data'], $data );

		if ( isset( $data['postid'] ) && ! empty( $data['postid'] ) ) {
			if(isset($data['item_layout_save']) && $data['item_layout_save'][0] == 'layout_part'){
				$response = $this->save_as_layout_part($data);
			} else {
				$response = $this->save_as_normal($data);
			}
		}

		wp_send_json( $response );
	}
	
	/**
	 * Save the item as Row or Module.
	 * 
	 * @access public
	 * Return Array
	 */
	 function save_as_normal($data){
		global $ThemifyBuilder;

		$title = isset( $data['item_title_field'] ) && ! empty( $data['item_title_field'] ) ? sanitize_text_field( $data['item_title_field'] ) : $this->user . ' Saved-'.ucwords(sanitize_text_field($data['type']));

		$data['item'] = @base64_decode(stripslashes($data['item']));
		$new_id = wp_insert_post(array(
				'post_status' => 'publish',
				'post_type' => $this->post_type_name[$data['type']],
				'post_author' => $this->user,
				'post_title' => $title,
				'post_content' => $data['item']
			));
		if ( $new_id ) {
			return array(
					'status' => 'success',
					'msg' => ''
				);
		} else
			return array(
					'status' => 'failed',
					'msg' => __('Something went wrong', 'themify')
				);
	 }
	
	/**
	 * Save the item as Layout Part.
	 * 
	 * @access public
	 * Return Array
	 */
	function save_as_layout_part($data){
		global $ThemifyBuilder_Data_Manager;
		
		$title = isset( $data['item_title_field'] ) && ! empty( $data['item_title_field'] ) ? sanitize_text_field( $data['item_title_field'] ) : __('Saved Item Layout Part','themify');

		$new_id = wp_insert_post(array(
			'post_status' => 'publish',
			'post_type' => $this->post_type_name['part'],
			'post_author' => $this->user,
			'post_title' => $title,
			'post_content' => ''
		));

		if(!$new_id){
			return array(
					'status' => 'failed',
					'msg' => __('Something went wrong while saving as builder layout part', 'themify')
				);
		}

		$data['item'] = json_decode(stripslashes_deep(base64_decode($data['item'])),true);
		if($data['type'] == 'module'){

			$row = array("row_order" => 0, "gutter" => "gutter-default", "column_alignment" => "col_align_top", "cols" => array(0 => array("column_order" => 0, "grid_class" => "col-full first last", "grid_width" => "", "modules" => array(), "styling" => array())), "styling" => array());
			$row['cols'][0]['modules'][1] = $data['item'];
			
			$ThemifyBuilder_Data_Manager->save_data( array($row), $new_id );

		} else {
			$ThemifyBuilder_Data_Manager->save_data( array( $data['item'] ), $new_id );
		}
		$status = array(
				'status' => 'success',
				'msg' => '',
				'model' => $data['model'],
				'replWith' => $this->get_layout_part_model($new_id,$data['type']),
				'opt' => $this->get_layout_part_list()
			);
			
		return $status;
	}

	/**
	 * Get layout part module settings.
	 * 
	 * @access Private
	 * Retrun Array
	 */
	private function get_layout_part_model($layout_part_id, $type){
		
		$layout_part = get_post($layout_part_id);
		
		$output = json_decode('{"mod_name":"layout-part","mod_settings":{
								"selected_layout_part":"'. $layout_part->post_name .'",
								"visibility_desktop_hide":"hide","visibility_tablet_hide":"hide","visibility_mobile_hide":"hide"}
							}', true );
		if($type == 'row'){
			$temp = array("row_order" => 0, "cols" => array("0" => array("column_order" => 0, "grid_class" => "col-full first last", "modules" => array(), "styling" => array())), "styling" => array());
			$temp['cols'][0]['modules'][1] = $output;
			$output = $temp;
		}
		
		return $output;
	}

	/**
	 * Get list of Saved Layout Parts in library.
	 * 
	 * @access Private
	 * Retrun Array or String
	 */
	private function get_layout_part_list($opt = true){
		global $wpdb;
		
		$results = $wpdb->get_results( $wpdb->prepare( "SELECT post_name, post_title FROM {$wpdb->posts} WHERE post_type = %s and post_status = 'publish'", $this->post_type_name['part'] ), ARRAY_A );
    
		if ( ! $results )
			return '';

		if ( ! $opt )
			return $results;
		
		$output = '<option></option>';

		foreach( $results as $index => $post ) {
			$output .= '<option value="' . $post['post_name'] . '">' . $post['post_title'] . '</option>';
		}
		
		return $output;
	}

	/**
	 * Get list of Saved Rows & Modules in library.
	 * 
	 * @access public
	 */
	public function list_library_items_ajax() {
		
		check_ajax_referer( 'tb_load_nonce', 'nonce' );
		
		$pid = $_POST['pid'];
		$rows = array();
		global $post;

		$posts = new WP_Query( array(
			'post_type' => $this->post_type_name,
			'posts_per_page' => -1,
			'orderby' => 'title',
			'order' => 'ASC',
			'post__not_in' => array($pid)
		));

		if( $posts->have_posts() ) : 

			while( $posts->have_posts() ) : 

				$posts->the_post();
				
				if(get_post_type() == $this->post_type_name['part']){
					$content = json_decode('{"mod_name":"layout-part","mod_settings":{
												"selected_layout_part":"'. $post->post_name .'",
												"visibility_desktop_hide":"hide","visibility_tablet_hide":"hide","visibility_mobile_hide":"hide"}
											}', true );
				} else {
					$content = json_decode( get_the_content() , true );
				}

				$type = array_search(get_post_type(),$this->post_type_name);

					array_push( $rows , array(
						'title' => get_the_title(),
						'pid'	=> get_the_ID(),
						'slug' => $post->post_name,
						'raw' => $content,
						'type' => $type
						)
					);

			endwhile;
		endif;
		wp_reset_postdata();

		wp_send_json($rows);
	}

	public function remove_library_item_ajax(){
		check_ajax_referer( 'tb_load_nonce', 'nonce' );

		$pid = (int)$_POST['pid'];
		$post = get_post($pid);
		$msg = array('status' => 0);

		if(in_array($post->post_type,$this->post_type_name)){
			if($this->post_type_name['part'] == $post->post_type){
				$msg['status'] = (bool)wp_trash_post($pid);
				$msg['opt'] = $this->get_layout_part_list();
				$msg['part'] = $post->post_name;
			} else {
				$msg['status'] = (bool)wp_delete_post($pid);
			}
		}

		wp_send_json($msg);
	}
}