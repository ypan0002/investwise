<?php

defined( 'ABSPATH' ) or die;

$GLOBALS['processed_terms'] = array();
$GLOBALS['processed_posts'] = array();

require_once ABSPATH . 'wp-admin/includes/post.php';
require_once ABSPATH . 'wp-admin/includes/taxonomy.php';
require_once ABSPATH . 'wp-admin/includes/image.php';

function themify_import_post( $post ) {
	global $processed_posts, $processed_terms;

	if ( ! post_type_exists( $post['post_type'] ) ) {
		return;
	}

	/* Menu items don't have reliable post_title, skip the post_exists check */
	if( $post['post_type'] !== 'nav_menu_item' ) {
		$post_exists = post_exists( $post['post_title'], '', $post['post_date'] );
		if ( $post_exists && get_post_type( $post_exists ) == $post['post_type'] ) {
			$processed_posts[ intval( $post['ID'] ) ] = intval( $post_exists );
			return;
		}
	}

	if( $post['post_type'] == 'nav_menu_item' ) {
		if( ! isset( $post['tax_input']['nav_menu'] ) || ! term_exists( $post['tax_input']['nav_menu'], 'nav_menu' ) ) {
			return;
		}
		$_menu_item_type = $post['meta_input']['_menu_item_type'];
		$_menu_item_object_id = $post['meta_input']['_menu_item_object_id'];

		if ( 'taxonomy' == $_menu_item_type && isset( $processed_terms[ intval( $_menu_item_object_id ) ] ) ) {
			$post['meta_input']['_menu_item_object_id'] = $processed_terms[ intval( $_menu_item_object_id ) ];
		} else if ( 'post_type' == $_menu_item_type && isset( $processed_posts[ intval( $_menu_item_object_id ) ] ) ) {
			$post['meta_input']['_menu_item_object_id'] = $processed_posts[ intval( $_menu_item_object_id ) ];
		} else if ( 'custom' != $_menu_item_type ) {
			// associated object is missing or not imported yet, we'll retry later
			// $missing_menu_items[] = $item;
			return;
		}
	}

	$post_parent = ( $post['post_type'] == 'nav_menu_item' ) ? $post['meta_input']['_menu_item_menu_item_parent'] : (int) $post['post_parent'];
	$post['post_parent'] = 0;
	if ( $post_parent ) {
		// if we already know the parent, map it to the new local ID
		if ( isset( $processed_posts[ $post_parent ] ) ) {
			if( $post['post_type'] == 'nav_menu_item' ) {
				$post['meta_input']['_menu_item_menu_item_parent'] = $processed_posts[ $post_parent ];
			} else {
				$post['post_parent'] = $processed_posts[ $post_parent ];
			}
		}
	}

	/**
	 * for hierarchical taxonomies, IDs must be used so wp_set_post_terms can function properly
	 * convert term slugs to IDs for hierarchical taxonomies
	 */
	if( ! empty( $post['tax_input'] ) ) {
		foreach( $post['tax_input'] as $tax => $terms ) {
			if( is_taxonomy_hierarchical( $tax ) ) {
				$terms = explode( ', ', $terms );
				$post['tax_input'][ $tax ] = array_map( 'themify_get_term_id_by_slug', $terms, array_fill( 0, count( $terms ), $tax ) );
			}
		}
	}

	$post['post_author'] = (int) get_current_user_id();
	$post['post_status'] = 'publish';

	$old_id = $post['ID'];

	unset( $post['ID'] );
	$post_id = wp_insert_post( $post, true );
	if( is_wp_error( $post_id ) ) {
		return false;
	} else {
		$processed_posts[ $old_id ] = $post_id;

		if( isset( $post['has_thumbnail'] ) && $post['has_thumbnail'] ) {
			$placeholder = themify_get_placeholder_image();
			if( ! is_wp_error( $placeholder ) ) {
				set_post_thumbnail( $post_id, $placeholder );
			}
		}

		return $post_id;
	}
}

function themify_get_placeholder_image() {
	static $placeholder_image = null;

	if( $placeholder_image == null ) {
		if ( ! function_exists( 'WP_Filesystem' ) ) {
			require_once ABSPATH . 'wp-admin/includes/file.php';
		}
		WP_Filesystem();
		global $wp_filesystem;
		$upload = wp_upload_bits( $post['post_name'] . '.jpg', null, $wp_filesystem->get_contents( THEMIFY_DIR . '/img/image-placeholder.jpg' ) );

		if ( $info = wp_check_filetype( $upload['file'] ) )
			$post['post_mime_type'] = $info['type'];
		else
			return new WP_Error( 'attachment_processing_error', __( 'Invalid file type', 'themify' ) );

		$post['guid'] = $upload['url'];
		$post_id = wp_insert_attachment( $post, $upload['file'] );
		wp_update_attachment_metadata( $post_id, wp_generate_attachment_metadata( $post_id, $upload['file'] ) );

		$placeholder_image = $post_id;
	}

	return $placeholder_image;
}

function themify_import_term( $term ) {
	global $processed_terms;

	if( $term_id = term_exists( $term['slug'], $term['taxonomy'] ) ) {
		if ( is_array( $term_id ) ) $term_id = $term_id['term_id'];
		if ( isset( $term['term_id'] ) )
			$processed_terms[ intval( $term['term_id'] ) ] = (int) $term_id;
		return (int) $term_id;
	}

	if ( empty( $term['parent'] ) ) {
		$parent = 0;
	} else {
		$parent = term_exists( $term['parent'], $term['taxonomy'] );
		if ( is_array( $parent ) ) $parent = $parent['term_id'];
	}

	$id = wp_insert_term( $term['name'], $term['taxonomy'], array(
		'parent' => $parent,
		'slug' => $term['slug'],
		'description' => $term['description'],
	) );
	if ( ! is_wp_error( $id ) ) {
		if ( isset( $term['term_id'] ) ) {
			$processed_terms[ intval($term['term_id']) ] = $id['term_id'];
			return $term['term_id'];
		}
	}

	return false;
}

function themify_get_term_id_by_slug( $slug, $tax ) {
	$term = get_term_by( 'slug', $slug, $tax );
	if( $term ) {
		return $term->term_id;
	}

	return false;
}

function themify_undo_import_term( $term ) {
	$term_id = term_exists( $term['slug'], $term['taxonomy'] );
	if ( $term_id ) {
		if ( is_array( $term_id ) ) $term_id = $term_id['term_id'];
		if ( isset( $term_id ) ) {
			wp_delete_term( $term_id, $term['taxonomy'] );
		}
	}
}

/**
 * Determine if a post exists based on title, content, and date
 *
 * @global wpdb $wpdb WordPress database abstraction object.
 *
 * @param array $args array of database parameters to check
 * @return int Post ID if post exists, 0 otherwise.
 */
function themify_post_exists( $args = array() ) {
	global $wpdb;

	$query = "SELECT ID FROM $wpdb->posts WHERE 1=1";
	$db_args = array();

	foreach ( $args as $key => $value ) {
		$value = wp_unslash( sanitize_post_field( $key, $value, 0, 'db' ) );
		if( ! empty( $value ) ) {
			$query .= ' AND ' . $key . ' = %s';
			$db_args[] = $value;
		}
	}

	if ( !empty ( $args ) )
		return (int) $wpdb->get_var( $wpdb->prepare($query, $args) );

	return 0;
}

function themify_undo_import_post( $post ) {
	if( $post['post_type'] == 'nav_menu_item' ) {
		$post_exists = themify_post_exists( array(
			'post_name' => $post['post_name'],
			'post_modified' => $post['post_date'],
			'post_type' => 'nav_menu_item',
		) );
	} else {
		$post_exists = post_exists( $post['post_title'], '', $post['post_date'] );
	}
	if( $post_exists && get_post_type( $post_exists ) == $post['post_type'] ) {
		/**
		 * check if the post has been modified, if so leave it be
		 *
		 * NOTE: posts are imported using wp_insert_post() which modifies post_modified field
		 * to be the same as post_date, hence to check if the post has been modified,
		 * the post_modified field is compared against post_date in the original post.
		 */
		if( $post['post_date'] == get_post_field( 'post_modified', $post_exists ) ) {
			wp_delete_post( $post_exists, true ); // true: bypass trash
		}
	}
}

function themify_do_demo_import() {

	if ( isset( $GLOBALS["ThemifyBuilder_Data_Manager"] ) ) {
		remove_action( "save_post", array( $GLOBALS["ThemifyBuilder_Data_Manager"], "save_builder_text_only"), 10, 3 );
	}
$term = array (
  'term_id' => 10,
  'name' => 'Cosmetic',
  'slug' => 'cosmetic',
  'term_group' => 0,
  'taxonomy' => 'product_cat',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 11,
  'name' => 'Cosmetic',
  'slug' => 'cosmetic',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 12,
  'name' => 'Anti Acne',
  'slug' => 'anti-acne',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 13,
  'name' => 'Soap',
  'slug' => 'soap',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 14,
  'name' => 'Supplement',
  'slug' => 'supplement',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 15,
  'name' => 'Aromatherapy',
  'slug' => 'aromatherapy',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 16,
  'name' => 'Shampoo',
  'slug' => 'shampoo',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 17,
  'name' => 'Shaving',
  'slug' => 'shaving',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 18,
  'name' => 'Cream',
  'slug' => 'cream',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 19,
  'name' => 'Nail Paint',
  'slug' => 'nail-paint',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 20,
  'name' => 'Waxing',
  'slug' => 'waxing',
  'term_group' => 0,
  'taxonomy' => 'product_tag',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$term = array (
  'term_id' => 2,
  'name' => 'Main Navigation',
  'slug' => 'main-navigation',
  'term_group' => 0,
  'taxonomy' => 'nav_menu',
  'description' => '',
  'parent' => 0,
);

if( ERASEDEMO ) {
	themify_undo_import_term( $term );
} else {
	themify_import_term( $term );
}

$post = array (
  'ID' => 6,
  'post_date' => '2016-11-02 02:37:31',
  'post_date_gmt' => '2016-11-02 02:37:31',
  'post_content' => '<!--themify_builder_static--><h1>Ultra Spa</h1><h4>The best rated spa in Toronto</h4>
 <h3>416-366 2886</h3>
 <ul> <li><strong>Mon &#8211; Fri</strong> <em>11:00am &#8211; 8:00pm</em></li> <li><strong>Sat</strong> <em>9:00am &#8211; 8:00pm</em></li> <li><strong>Sun</strong> <em>9:00am &#8211; 5:00pm</em></li> </ul>
<h2>Services<br/></h2>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/nails-400x340.jpg" width="400" height="340" alt="Nails" /> 
 <h3 style="text-align: center;">Nails</h3><p style="text-align: center;">Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud.</p>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-360x480.jpg" width="360" height="480" alt="Facial" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/facial.jpg 360w, https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-225x300.jpg 225w" sizes="(max-width: 360px) 100vw, 360px" /> 
 <h3>Facial</h3> <p>With endless modalities and customization, we have thousands of spa and wellness location for your next massage</p> <ul> <li> Lorem ipsum dolor sit amet.</li> <li> Duis aute irure dolor.</li> <li> Nemo enim ipsam voluptatem.</li> <li> Temporibus autem quibusdam.</li> </ul>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-560x340.jpg" width="560" height="340" alt="massage" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/massage.jpg 560w, https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-300x182.jpg 300w" sizes="(max-width: 560px) 100vw, 560px" /> 
 <h3 style="text-align: right;">Massage</h3><p style="text-align: right;">Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda.</p>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-360x240.jpg" width="360" height="240" alt="waxing" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing.jpg 360w, https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-300x200.jpg 300w" sizes="(max-width: 360px) 100vw, 360px" /> 
 <h3>Waxing</h3><p>Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet.</p>
 
 <a href="http://themify.me/" > View service & pricing </a> 
 <h2 style="text-align: right;">Promotions</h2> <p style="text-align: right;">Subscribe our newsletter and receive monthly promotions &#038; new products.</p>
 <h2>Get 15% discount</h2><form id="mc4wp-form-1" method="post" data-id="130" data-name="" ><p>
 <label>Email address: </label>
 <input type="email" name="EMAIL" placeholder="Email Address" required />
 <input type="submit" value="Subscribe" />
</p><label style="display: none !important;">Leave this field empty if you\'re human: <input type="text" name="_mc4wp_honeypot" value="" tabindex="-1" autocomplete="off" /></label><input type="hidden" name="_mc4wp_timestamp" value="1522273128" /><input type="hidden" name="_mc4wp_form_id" value="130" /><input type="hidden" name="_mc4wp_form_element_id" value="mc4wp-form-1" /></form>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/title-white-divider.png" alt="title white divider" /> 
 <h2>This Month Special</h2> 
 
 $24 <p>Beauty</p> <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.</p> <p></p> 
 
 $15 <p>Massage</p> <p>Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.</p> <p></p> 
 
 $15 <p>Yoga</p> <p>Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.</p> <p></p> 
 <h5 style="text-align: center;">Visit our store &#038; present your code:<br />WEBPROMO</h5>
<h2>Our Shop<br/></h2>
 
 
 
 <figure> <a href="https://themify.me/demo/themes/ultra-spa/product/cosmetic-set/"><img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/pink-cosmetic-258x243.jpg" width="258" height="243" alt="pink-cosmetic" /></a> </figure> 
 <h3><a href="https://themify.me/demo/themes/ultra-spa/product/cosmetic-set/" title="Cosmetic Set">Cosmetic Set</a></h3> 
 &#36;70.00 <p><a href="/demo/themes/ultra-spa/wp-admin/admin-ajax.php?add-to-cart=87" data-quantity="1" data-product_id="87" data-product_sku="" aria-label="Add &ldquo;Cosmetic Set&rdquo; to your cart" rel="nofollow">Add to cart</a></p> [<a href="https://themify.me/demo/themes/ultra-spa/wp-admin/post.php?post=87&#038;action=edit">Edit</a>] 
 
 <figure> <a href="https://themify.me/demo/themes/ultra-spa/product/anti-acne-cream-package/"><img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/anti-acne-cream-package-258x243.jpg" width="258" height="243" alt="anti-acne-cream-package" /></a> </figure> 
 <h3><a href="https://themify.me/demo/themes/ultra-spa/product/anti-acne-cream-package/" title="Anti Acne Cream Package">Anti Acne Cream Package</a></h3> 
 &#36;150.00 <p><a href="/demo/themes/ultra-spa/wp-admin/admin-ajax.php?add-to-cart=85" data-quantity="1" data-product_id="85" data-product_sku="" aria-label="Add &ldquo;Anti Acne Cream Package&rdquo; to your cart" rel="nofollow">Add to cart</a></p> [<a href="https://themify.me/demo/themes/ultra-spa/wp-admin/post.php?post=85&#038;action=edit">Edit</a>] 
 
 <figure> <a href="https://themify.me/demo/themes/ultra-spa/product/bath-salt/"><img src="https://themify.me/demo/themes/ultra-spa/files/2016/10/olive-soap-258x243.jpg" width="258" height="243" alt="olive-soap" /></a> </figure> 
 <h3><a href="https://themify.me/demo/themes/ultra-spa/product/bath-salt/" title="Bath Salt">Bath Salt</a></h3> 
 &#36;25.00 <p><a href="/demo/themes/ultra-spa/wp-admin/admin-ajax.php?add-to-cart=49" data-quantity="1" data-product_id="49" data-product_sku="" aria-label="Add &ldquo;Bath Salt&rdquo; to your cart" rel="nofollow">Add to cart</a></p> [<a href="https://themify.me/demo/themes/ultra-spa/wp-admin/post.php?post=49&#038;action=edit">Edit</a>] 
 
 <figure> <a href="https://themify.me/demo/themes/ultra-spa/product/skin-supplement/"><img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/skin-suplement-258x243.jpg" width="258" height="243" alt="skin-suplement" /></a> </figure> 
 <h3><a href="https://themify.me/demo/themes/ultra-spa/product/skin-supplement/" title="Skin Supplement">Skin Supplement</a></h3> 
 &#36;25.00 <p><a href="/demo/themes/ultra-spa/wp-admin/admin-ajax.php?add-to-cart=81" data-quantity="1" data-product_id="81" data-product_sku="" aria-label="Add &ldquo;Skin Supplement&rdquo; to your cart" rel="nofollow">Add to cart</a></p> [<a href="https://themify.me/demo/themes/ultra-spa/wp-admin/post.php?post=81&#038;action=edit">Edit</a>] 
 
 
 
 <a href="https://themify.me/demo/themes/ultra-spa/shop" > Shop More </a><!--/themify_builder_static-->',
  'post_title' => 'Home',
  'post_excerpt' => '',
  'post_name' => 'home',
  'post_modified' => '2018-03-28 21:38:48',
  'post_modified_gmt' => '2018-03-28 21:38:48',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?page_id=6',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
    'page_layout' => 'sidebar-none',
    'content_width' => 'full_width',
    'hide_page_title' => 'yes',
    'header_design' => 'header-horizontal',
    'header_wrap' => 'transparent',
    'builder_switch_frontend' => '0',
    '_themify_builder_settings_json' => '[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h1>Ultra Spa<\\\\/h1><h4>The best rated spa in Toronto<\\\\/h4>\\",\\"background_image-type\\":\\"image\\",\\"font_size\\":\\"1.1\\",\\"font_size_unit\\":\\"em\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_h1\\":\\"4.75\\",\\"font_size_h1_unit\\":\\"em\\",\\"line_height_h1\\":\\"72\\",\\"font_size_h4\\":\\"1.5\\",\\"font_size_h4_unit\\":\\"em\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c17\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3>416-366 2886<\\\\/h3>\\",\\"background_image-type\\":\\"image\\",\\"font_color\\":\\"f1a4a4_1.00\\",\\"font_size\\":\\"1.2\\",\\"font_size_unit\\":\\"em\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c21\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<p>[themify_list style=\\\\\\\\\\\\\\"location-hours\\\\\\\\\\\\\\"]<\\\\/p>\\\\n<ul>\\\\n<li><strong>Mon - Fri<\\\\/strong> <em>11:00am - 8:00pm<\\\\/em><\\\\/li>\\\\n<li><strong>Sat<\\\\/strong> <em>9:00am - 8:00pm<\\\\/em><\\\\/li>\\\\n<li><strong>Sun<\\\\/strong> <em>9:00am - 5:00pm<\\\\/em><\\\\/li>\\\\n<\\\\/ul>\\\\n<p>[\\\\/themify_list]<\\\\/p>\\\\n\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c25\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"spa-info\\"}},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-2\\"}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider_size\\":\\"large\\",\\"background_slider_mode\\":\\"fullcover\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/ultra-spa-header-2-1024x716.jpg\\",\\"background_repeat\\":\\"repeat-none\\",\\"background_attachment\\":\\"scroll\\",\\"background_position\\":\\"right-bottom\\",\\"background_color\\":\\"eefeff\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"20\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"20\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"1\\",\\"breakpoint_tablet_landscape\\":{\\"background_type\\":\\"image\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/bg-header-spa-mobile-4.jpg\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_tablet\\":{\\"background_type\\":\\"image\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/bg-header-spa-mobile-4.jpg\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/bg-header-spa-mobile-small.jpg\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"25\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"50\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"1\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Services\\",\\"heading_tag\\":\\"h2\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_bottom\\":\\"50\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c40\\"}},{\\"row_order\\":\\"1\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-center\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/nails-400x340.jpg\\",\\"width_image\\":\\"400\\",\\"height_image\\":\\"340\\",\\"param_image\\":\\"regular\\",\\"alt_image\\":\\"Nails\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c52\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3 style=\\\\\\\\\\\\\\"text-align: center;\\\\\\\\\\\\\\">Nails<\\\\/h3><p style=\\\\\\\\\\\\\\"text-align: center;\\\\\\\\\\\\\\">Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud.<\\\\/p>\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_mobile\\":{\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"60\\",\\"margin_bottom\\":\\"30\\"},\\"cid\\":\\"c56\\"}}]},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-center\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/facial.jpg\\",\\"width_image\\":\\"360\\",\\"height_image\\":\\"480\\",\\"param_image\\":\\"regular\\",\\"alt_image\\":\\"Facial\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c64\\"}}]},{\\"column_order\\":\\"2\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3>Facial<\\\\/h3>\\\\n<p>With endless modalities and customization, we have thousands of spa and wellness location for your next massage<\\\\/p>\\\\n<ul>\\\\n<li><i class=\\\\\\\\\\\\\\"fa fa-circle\\\\\\\\\\\\\\"><\\\\/i> Lorem ipsum dolor sit amet.<\\\\/li>\\\\n<li><i class=\\\\\\\\\\\\\\"fa fa-circle\\\\\\\\\\\\\\"><\\\\/i> Duis aute irure dolor.<\\\\/li>\\\\n<li><i class=\\\\\\\\\\\\\\"fa fa-circle\\\\\\\\\\\\\\"><\\\\/i> Nemo enim ipsam voluptatem.<\\\\/li>\\\\n<li><i class=\\\\\\\\\\\\\\"fa fa-circle\\\\\\\\\\\\\\"><\\\\/i> Temporibus autem quibusdam.<\\\\/li>\\\\n<\\\\/ul>\\\\n\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"breakpoint_mobile\\":{\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"margin_bottom\\":\\"30\\"},\\"cid\\":\\"c72\\"}}]}]}]}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/pink-flower-1.jpg\\",\\"background_repeat\\":\\"repeat-none\\",\\"background_position\\":\\"left-center\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"5\\",\\"padding_top_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_tablet_landscape\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/pink-flower-small-mobile.jpg\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"20\\",\\"padding_top_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"2\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-center\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/massage.jpg\\",\\"width_image\\":\\"560\\",\\"height_image\\":\\"340\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c91\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3 style=\\\\\\\\\\\\\\"text-align: right;\\\\\\\\\\\\\\">Massage<\\\\/h3><p style=\\\\\\\\\\\\\\"text-align: right;\\\\\\\\\\\\\\">Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda.<\\\\/p>\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_mobile\\":{\\"background_image-type\\":\\"image\\",\\"text_align\\":\\"center\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"margin_bottom\\":\\"30\\"},\\"cid\\":\\"c95\\"}}]},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-top\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/waxing.jpg\\",\\"width_image\\":\\"360\\",\\"height_image\\":\\"240\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c103\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3>Waxing<\\\\/h3><p>Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet.<\\\\/p>\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_mobile\\":{\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"margin_bottom\\":\\"40\\"},\\"cid\\":\\"c107\\"}}]}]},{\\"mod_name\\":\\"buttons\\",\\"mod_settings\\":{\\"background_image-type\\":\\"image\\",\\"background_repeat\\":\\"repeat\\",\\"text_align\\":\\"center\\",\\"checkbox_padding_apply_all\\":\\"1\\",\\"margin_top\\":\\"30\\",\\"border_top_style\\":\\"none\\",\\"border_right_style\\":\\"none\\",\\"border_bottom_style\\":\\"none\\",\\"border_left_style\\":\\"none\\",\\"checkbox_border_apply_all\\":\\"1\\",\\"link_color\\":\\"ffffff\\",\\"padding_link_top\\":\\"5\\",\\"padding_link_top_unit\\":\\"%\\",\\"checkbox_padding_link_apply_all\\":\\"1\\",\\"checkbox_link_margin_apply_all\\":\\"1\\",\\"checkbox_link_border_apply_all\\":\\"1\\",\\"buttons_size\\":\\"large\\",\\"buttons_style\\":\\"squared\\",\\"content_button\\":[{\\"label\\":\\"View service & pricing\\",\\"link\\":\\"http:\\\\/\\\\/themify.me\\\\/\\",\\"link_options\\":\\"regular\\",\\"button_color_bg\\":\\"pink\\"}]}}]}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider_size\\":\\"large\\",\\"background_slider_mode\\":\\"fullcover\\",\\"background_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/white-flower.jpg\\",\\"background_repeat\\":\\"repeat-none\\",\\"background_attachment\\":\\"scroll\\",\\"background_position\\":\\"right-bottom\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_bottom\\":\\"20\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"1\\",\\"breakpoint_tablet_landscape\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_bottom\\":\\"20\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"1\\"}}},{\\"row_order\\":\\"3\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h2 style=\\\\\\\\\\\\\\"text-align: right;\\\\\\\\\\\\\\">Promotions<\\\\/h2>\\\\n<p style=\\\\\\\\\\\\\\"text-align: right;\\\\\\\\\\\\\\">Subscribe our newsletter and receive monthly promotions &amp; new products.<\\\\/p>\\\\n\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c122\\"}}],\\"grid_width\\":\\"33.1412\\",\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"font_color\\":\\"ffffff_1.00\\",\\"padding_right\\":\\"5\\",\\"padding_right_unit\\":\\"%\\",\\"border_right_color\\":\\"47556c_1.00\\",\\"border_right_width\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"border\\"}},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h2>Get 15% discount<\\\\/h2><p>[mc4wp_form id=\\\\\\\\\\\\\\"130\\\\\\\\\\\\\\"]<\\\\/p>\\",\\"add_css_text\\":\\"news-letter\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c130\\"}}],\\"grid_width\\":\\"63.6598\\",\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"font_color\\":\\"ffffff_1.00\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_border_apply_all\\":\\"border\\"}}],\\"styling\\":{\\"custom_css_row\\":\\"newsletter-block\\",\\"background_type\\":\\"image\\",\\"background_color\\":\\"273244_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"5\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"12\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"40\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"4\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-center\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/title-white-divider.png\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_bottom\\":\\"10\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c141\\"}},{\\"mod_name\\":\\"plain-text\\",\\"mod_settings\\":{\\"plain_text\\":\\"<h2>This Month Special<\\\\/h2>\\",\\"background_image-type\\":\\"image\\",\\"font_color\\":\\"ffffff_1.00\\",\\"text_align\\":\\"center\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"0\\",\\"margin_right\\":\\"0\\",\\"margin_bottom\\":\\"-10\\",\\"margin_left\\":\\"0\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c145\\"}},{\\"row_order\\":\\"2\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"pricing-table\\",\\"mod_settings\\":{\\"mod_color_pricing_table\\":\\"transparent\\",\\"mod_price_pricing_table\\":\\"$24\\",\\"mod_description_pricing_table\\":\\"Beauty\\",\\"mod_feature_list_pricing_table\\":\\"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\\",\\"border_right_color\\":\\"a1e8ee_1.00\\",\\"border_right_width\\":\\"1\\",\\"mod_title_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_title_left\\":\\"left\\",\\"mod_text_align_title_center\\":\\"center\\",\\"mod_text_align_title_right\\":\\"right\\",\\"mod_text_align_title_justify\\":\\"justify\\",\\"mod_title_background_color\\":\\"82d3da_1.00\\",\\"mod_feature_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_content_left\\":\\"left\\",\\"mod_text_align_content_center\\":\\"center\\",\\"mod_text_align_content_right\\":\\"right\\",\\"mod_text_align_content_justify\\":\\"justify\\",\\"mod_feature_bg_color\\":\\"82d3da_1.00\\",\\"mod_text_align_button_left\\":\\"left\\",\\"mod_text_align_button_center\\":\\"center\\",\\"mod_text_align_button_right\\":\\"right\\",\\"mod_text_align_button_justify\\":\\"justify\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c157\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_border_apply_all\\":\\"border\\"}},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"pricing-table\\",\\"mod_settings\\":{\\"mod_color_pricing_table\\":\\"transparent\\",\\"mod_price_pricing_table\\":\\"$15\\",\\"mod_description_pricing_table\\":\\"Massage\\",\\"mod_feature_list_pricing_table\\":\\"Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam.\\",\\"border_right_color\\":\\"a1e8ee_1.00\\",\\"border_right_width\\":\\"1\\",\\"mod_title_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_title_left\\":\\"left\\",\\"mod_text_align_title_center\\":\\"center\\",\\"mod_text_align_title_right\\":\\"right\\",\\"mod_text_align_title_justify\\":\\"justify\\",\\"mod_title_background_color\\":\\"82d3da_1.00\\",\\"mod_feature_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_content_left\\":\\"left\\",\\"mod_text_align_content_center\\":\\"center\\",\\"mod_text_align_content_right\\":\\"right\\",\\"mod_text_align_content_justify\\":\\"justify\\",\\"mod_feature_bg_color\\":\\"82d3da_1.00\\",\\"mod_text_align_button_left\\":\\"left\\",\\"mod_text_align_button_center\\":\\"center\\",\\"mod_text_align_button_right\\":\\"right\\",\\"mod_text_align_button_justify\\":\\"justify\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c165\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\"}},{\\"column_order\\":\\"2\\",\\"grid_class\\":\\"col3-1\\",\\"modules\\":[{\\"mod_name\\":\\"pricing-table\\",\\"mod_settings\\":{\\"mod_color_pricing_table\\":\\"transparent\\",\\"mod_price_pricing_table\\":\\"$15\\",\\"mod_description_pricing_table\\":\\"Yoga\\",\\"mod_feature_list_pricing_table\\":\\"Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.\\",\\"mod_title_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_title_left\\":\\"left\\",\\"mod_text_align_title_center\\":\\"center\\",\\"mod_text_align_title_right\\":\\"right\\",\\"mod_text_align_title_justify\\":\\"justify\\",\\"mod_title_background_color\\":\\"82d3da_1.00\\",\\"mod_feature_font_color\\":\\"ffffff_1.00\\",\\"mod_text_align_content_left\\":\\"left\\",\\"mod_text_align_content_center\\":\\"center\\",\\"mod_text_align_content_right\\":\\"right\\",\\"mod_text_align_content_justify\\":\\"justify\\",\\"mod_feature_bg_color\\":\\"82d3da_1.00\\",\\"mod_text_align_button_left\\":\\"left\\",\\"mod_text_align_button_center\\":\\"center\\",\\"mod_text_align_button_right\\":\\"right\\",\\"mod_text_align_button_justify\\":\\"justify\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c173\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_border_apply_all\\":\\"border\\"}}],\\"gutter\\":\\"gutter-none\\"},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h5 style=\\\\\\\\\\\\\\"text-align: center;\\\\\\\\\\\\\\">Visit our store &amp; present your code:<br \\\\/>WEBPROMO<\\\\/h5>\\",\\"background_image-type\\":\\"image\\",\\"font_color\\":\\"fff\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c177\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_color\\":\\"82d3da_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"50\\",\\"padding_right\\":\\"5\\",\\"padding_right_unit\\":\\"%\\",\\"padding_bottom\\":\\"50\\",\\"padding_left\\":\\"5\\",\\"padding_left_unit\\":\\"%\\",\\"checkbox_border_apply_all\\":\\"border\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"-150\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_tablet\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"5\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Our Shop\\",\\"heading_tag\\":\\"h2\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_bottom\\":\\"50\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c188\\"}},{\\"mod_name\\":\\"products\\",\\"mod_settings\\":{\\"query_products\\":\\"all\\",\\"category_products\\":\\"|single\\",\\"hide_child_products\\":\\"no\\",\\"hide_free_products\\":\\"no\\",\\"post_per_page_products\\":\\"4\\",\\"orderby_products\\":\\"date\\",\\"order_products\\":\\"desc\\",\\"template_products\\":\\"list\\",\\"layout_products\\":\\"grid4\\",\\"layout_slider\\":\\"slider-default\\",\\"visible_opt_slider\\":\\"1\\",\\"auto_scroll_opt_slider\\":\\"4\\",\\"scroll_opt_slider\\":\\"1\\",\\"speed_opt_slider\\":\\"normal\\",\\"effect_slider\\":\\"scroll\\",\\"pause_on_hover_slider\\":\\"resume\\",\\"wrap_slider\\":\\"yes\\",\\"show_nav_slider\\":\\"yes\\",\\"show_arrow_slider\\":\\"yes\\",\\"description_products\\":\\"none\\",\\"hide_feat_img_products\\":\\"no\\",\\"img_width_products\\":\\"258\\",\\"img_height_products\\":\\"243\\",\\"unlink_feat_img_products\\":\\"no\\",\\"hide_post_title_products\\":\\"no\\",\\"unlink_post_title_products\\":\\"no\\",\\"hide_price_products\\":\\"no\\",\\"hide_add_to_cart_products\\":\\"no\\",\\"hide_rating_products\\":\\"no\\",\\"hide_sales_badge\\":\\"no\\",\\"hide_page_nav_products\\":\\"yes\\",\\"text_align\\":\\"center\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"buttons\\",\\"mod_settings\\":{\\"buttons_size\\":\\"normal\\",\\"buttons_style\\":\\"outline\\",\\"content_button\\":[{\\"label\\":\\"Shop More\\",\\"link\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/shop\\",\\"link_options\\":\\"regular\\",\\"button_color_bg\\":\\"pink\\"}],\\"background_image-type\\":\\"image\\",\\"text_align\\":\\"center\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"padding_top_link\\":\\"20\\",\\"padding_right_link\\":\\"40\\",\\"padding_bottom_link\\":\\"20\\",\\"padding_left_link\\":\\"40\\",\\"checkbox_link_padding_apply_all\\":\\"padding\\",\\"link_checkbox_margin_apply_all\\":\\"margin\\",\\"link_checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c196\\"}}]}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"8\\",\\"padding_top_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}},{\\"row_order\\":\\"6\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\"}]}]',
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 8,
  'post_date' => '2016-11-02 02:38:09',
  'post_date_gmt' => '2016-11-02 02:38:09',
  'post_content' => '<!--themify_builder_static--><h1>Services<br/>What we offer</h1>
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/nails-560x325.jpg" width="560" height="325" title="Nails" alt="Nails" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/nails.jpg 560w, https://themify.me/demo/themes/ultra-spa/files/2016/11/nails-300x174.jpg 300w" sizes="(max-width: 560px) 100vw, 560px" /> <h3> Nails </h3> 
 
 <h4>Nail Manicure (Per packages)</h4> $25 <br /> 
 
 <h4>Hand Massage (Per packages)</h4> $35 <br /> 
 
 <h4>Nail Coloring (Per color)</h4> $25 <br /> 
 
 <h4>Nail Theraphy (50Min)</h4> $30 <br /> 
 
 <h4>Nail Painting (40Min)</h4> $15 <br /> 
 
 <h4>Customized Spa Manicure (40Min)</h4> $25 <br /> 
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-service-560x325.jpg" width="560" height="325" title="Massage" alt="Massage" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-service.jpg 560w, https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-service-300x174.jpg 300w" sizes="(max-width: 560px) 100vw, 560px" /> <h3> Massage </h3> 
 
 <h4>Full Body Massage (55 Min)</h4> $25 <br /> 
 
 <h4>Hot Stone Massage (55 Min)</h4> $25 <br /> 
 
 <h4>Deep Tissue Massage (Per area)</h4> $25 <br /> 
 
 <h4>Aromatherapy Massage (50 Min)</h4> $25 <br /> 
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-service-560x325.jpg" width="560" height="325" title="Facial" alt="Facial" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-service.jpg 560w, https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-service-300x174.jpg 300w" sizes="(max-width: 560px) 100vw, 560px" /> <h3> Facial </h3> 
 
 <h4>Face Scrub (45 Min)</h4> $25 <br /> 
 
 <h4>Face Massage (30 Min)</h4> $35 <br /> 
 
 <h4>Facial (Per packages)</h4> $25 <br /> 
 
 <img src="https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-service-560x325.jpg" width="560" height="325" title="Waxing" alt="Waxing" srcset="https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-service.jpg 560w, https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-service-300x174.jpg 300w" sizes="(max-width: 560px) 100vw, 560px" /> <h3> Waxing </h3> 
 
 <h4>Laser Wax (Per packages)</h4> $155 <br /> 
 
 <h4>Avocado wax (Per area)</h4> $25 <br /> 
 
 <h4>Apricott Wax (Per area)</h4> $35 <br /> 
 
 <h4>Natural Waxing (Per packages)</h4> $55 <br /> 
<h2>Call To Book Your Appoinment <br/>416 - 228 - 3368</h2>
 
 
 Bay st, Toronto, ON<!--/themify_builder_static-->',
  'post_title' => 'Services',
  'post_excerpt' => '',
  'post_name' => 'services',
  'post_modified' => '2017-11-01 17:28:31',
  'post_modified_gmt' => '2017-11-01 17:28:31',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?page_id=8',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
    'page_layout' => 'sidebar-none',
    'content_width' => 'full_width',
    'hide_page_title' => 'yes',
    'builder_switch_frontend' => '0',
    '_themify_builder_settings_json' => '[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Services\\",\\"sub_heading\\":\\"What we offer\\",\\"heading_tag\\":\\"h1\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c15\\"}}]}],\\"styling\\":{\\"row_width\\":\\"fullwidth\\",\\"background_type\\":\\"image\\",\\"background_color\\":\\"f4f4f4_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"4\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"3\\",\\"padding_bottom_unit\\":\\"%\\",\\"margin_top_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_bottom_color\\":\\"dddddd_1.00\\",\\"border_bottom_width\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"border\\"}},{\\"row_order\\":\\"1\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/nails.jpg\\",\\"width_image\\":\\"560\\",\\"height_image\\":\\"325\\",\\"title_image\\":\\"Nails\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"padding_bottom\\":\\"6\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title\\":\\"2.5\\",\\"font_size_title_unit\\":\\"em\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c26\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Nail Manicure (Per packages)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"6\\",\\"margin_top_unit\\":\\"%\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Hand Massage (Per packages)\\",\\"price_service_menu\\":\\"$35\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Nail Coloring (Per color)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Nail Theraphy (50Min)\\",\\"price_service_menu\\":\\"$30\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Nail Painting (40Min)\\",\\"price_service_menu\\":\\"$15\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Customized Spa Manicure (40Min)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/massage-service.jpg\\",\\"width_image\\":\\"560\\",\\"height_image\\":\\"325\\",\\"title_image\\":\\"Massage\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"padding_top\\":\\"10\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"6\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title\\":\\"2.5\\",\\"font_size_title_unit\\":\\"em\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c54\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Full Body Massage (55 Min)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"6\\",\\"margin_top_unit\\":\\"%\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Hot Stone Massage (55 Min)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Deep Tissue Massage (Per area)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Aromatherapy Massage (50 Min)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}}]},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-2\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/facial-service.jpg\\",\\"width_image\\":\\"560\\",\\"height_image\\":\\"325\\",\\"title_image\\":\\"Facial\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"padding_bottom\\":\\"6\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title\\":\\"2.5\\",\\"font_size_title_unit\\":\\"em\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c78\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Face Scrub (45 Min)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"6\\",\\"margin_top_unit\\":\\"%\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Face Massage (30 Min)\\",\\"price_service_menu\\":\\"$35\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Facial (Per packages)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/waxing-service.jpg\\",\\"width_image\\":\\"560\\",\\"height_image\\":\\"325\\",\\"title_image\\":\\"Waxing\\",\\"param_image\\":\\"regular\\",\\"background_image-type\\":\\"image\\",\\"padding_top\\":\\"10\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"6\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title\\":\\"2.5\\",\\"font_size_title_unit\\":\\"em\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c94\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Laser Wax (Per packages)\\",\\"price_service_menu\\":\\"$155\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"6\\",\\"margin_top_unit\\":\\"%\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Avocado wax (Per area)\\",\\"price_service_menu\\":\\"$25\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Apricott Wax (Per area)\\",\\"price_service_menu\\":\\"$35\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"service-menu\\",\\"mod_settings\\":{\\"style_service_menu\\":\\"image-top\\",\\"title_service_menu\\":\\"Natural Waxing (Per packages)\\",\\"price_service_menu\\":\\"$55\\",\\"link_options\\":\\"regular\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\"}}]}],\\"styling\\":{\\"custom_css_row\\":\\"service-block\\",\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"6\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"8\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}},{\\"row_order\\":\\"2\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Call To Book Your Appoinment \\",\\"sub_heading\\":\\"416 - 228 - 3368\\",\\"heading_tag\\":\\"h2\\",\\"css_class\\":\\"white-heading\\",\\"background_image-type\\":\\"image\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_color\\":\\"ffffff_1.00\\",\\"font_size\\":\\"36\\",\\"font_color_subheading\\":\\"ffffff_1.00\\",\\"font_size_subheading\\":\\"36\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"cid\\":\\"c121\\"}}]}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_color\\":\\"273244_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"4\\",\\"padding_top_unit\\":\\"%\\",\\"padding_bottom\\":\\"15\\",\\"padding_bottom_unit\\":\\"%\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_top\\":\\"40\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"3\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\",\\"modules\\":[{\\"mod_name\\":\\"maps-pro\\",\\"mod_settings\\":{\\"checkbox_padding_apply_all\\":\\"1\\",\\"checkbox_margin_apply_all\\":\\"1\\",\\"checkbox_border_apply_all\\":\\"1\\",\\"map_display_type\\":\\"dynamic\\",\\"w_map\\":\\"100\\",\\"unit_w\\":\\"%\\",\\"w_map_static\\":\\"500\\",\\"h_map\\":\\"545\\",\\"type_map\\":\\"ROADMAP\\",\\"style_map\\":\\"greyscale\\",\\"scrollwheel_map\\":\\"disable\\",\\"draggable_map\\":\\"enable\\",\\"disable_map_ui\\":\\"no\\",\\"zoom_map\\":\\"18\\",\\"map_center\\":\\"1 Yonge Street,\\\\nToronto, ON\\\\nCanada\\",\\"markers\\":[{\\"address\\":\\"1 Yonge Street,\\\\nToronto, ON\\\\nCanada\\",\\"latlng\\":\\"43.6427432,-79.37410460000001\\",\\"title\\":\\"Bay st, Toronto, ON\\",\\"image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/map-pin.png\\"}]}}]}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider_size\\":\\"large\\",\\"background_slider_mode\\":\\"fullcover\\",\\"background_repeat\\":\\"repeat\\",\\"background_position\\":\\"center-center\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"padding_bottom\\":\\"8\\",\\"padding_bottom_unit\\":\\"%\\",\\"margin_top\\":\\"-200\\",\\"checkbox_border_apply_all\\":\\"1\\",\\"breakpoint_tablet_landscape\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"-80\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_tablet\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"-50\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"cover_color-type\\":\\"color\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"-50\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"4\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full\\"}]}]',
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 10,
  'post_date' => '2016-11-02 02:38:29',
  'post_date_gmt' => '2016-11-02 02:38:29',
  'post_content' => '',
  'post_title' => 'Gallery',
  'post_excerpt' => '',
  'post_name' => 'gallery',
  'post_modified' => '2017-08-28 15:22:04',
  'post_modified_gmt' => '2017-08-28 15:22:04',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?page_id=10',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
    'page_layout' => 'sidebar-none',
    'content_width' => 'full_width',
    'hide_page_title' => 'yes',
    'builder_switch_frontend' => '0',
    '_themify_builder_settings_json' => '[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Gallery\\",\\"sub_heading\\":\\"Photos\\",\\"heading_tag\\":\\"h1\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"|\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"font_family_subheading\\":\\"default\\",\\"font_size_subheading_unit\\":\\"px\\",\\"line_height_subheading_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]}],\\"styling\\":{\\"row_width\\":\\"fullwidth\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"f4f4f4_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"4\\",\\"padding_top_unit\\":\\"%\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"3\\",\\"padding_bottom_unit\\":\\"%\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"em\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"dddddd_1.00\\",\\"border_bottom_width\\":\\"1\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\",\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"12\\",\\"padding_top_unit\\":\\"%\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"1\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first\\",\\"modules\\":[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-1 first\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID377634-476x353.jpg\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Lily Aromatheraphy\\",\\"link_image\\":\\"https://themify.me/demo/themes/ultra-spa/services/\\",\\"param_image\\":\\"newtab\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-gradient-angle\\":\\"180\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/massage-476x353-1.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Massage\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID1167254-476x353.jpg\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Rose Oil Theraphy\\",\\"link_image\\":\\"https://themify.me/demo/themes/ultra-spa/services/\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"zoom\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-gradient-angle\\":\\"180\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_style\\":\\"none\\",\\"border_right_style\\":\\"none\\",\\"border_bottom_style\\":\\"none\\",\\"border_left_style\\":\\"none\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID1301337-476x353.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Hot Stone Massage\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"zoom\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"none\\",\\"border_right_style\\":\\"none\\",\\"border_bottom_style\\":\\"none\\",\\"border_left_style\\":\\"none\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]},{\\"column_order\\":\\"2\\",\\"grid_class\\":\\"col4-2 last\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID376407-4-750x556.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"750\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"556\\",\\"title_image\\":\\"Stay Fresh\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]}],\\"gutter\\":\\"gutter-none\\",\\"column_alignment\\":\\"\\",\\"styling\\":[]}],\\"styling\\":[]}],\\"styling\\":{\\"row_width\\":\\"fullwidth\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"gallery-block\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\"}},{\\"row_order\\":\\"2\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first\\",\\"modules\\":[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2 first\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID377644-951x353.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"951\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Lavender Aromatheraphy\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID1301286-476x353.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Olive Oil \\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]},{\\"column_order\\":\\"2\\",\\"grid_class\\":\\"col4-1 last\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa/files/2016/11/PeopleImages.com-ID8214-476x353.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"476\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"353\\",\\"title_image\\":\\"Manicure and pedicure\\",\\"param_image\\":\\"lightbox\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]}],\\"gutter\\":\\"gutter-none\\",\\"column_alignment\\":\\"\\",\\"styling\\":[]}],\\"styling\\":[]}],\\"styling\\":{\\"row_width\\":\\"fullwidth\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"gallery-block\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\"}},{\\"row_order\\":\\"3\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first\\",\\"modules\\":[{\\"mod_name\\":\\"fancy-heading\\",\\"mod_settings\\":{\\"heading\\":\\"Instagram\\",\\"sub_heading\\":\\"Our Instagram Photo\\",\\"heading_tag\\":\\"h2\\",\\"text_alignment\\":\\"themify-text-center\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"|\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"font_family_subheading\\":\\"default\\",\\"font_size_subheading_unit\\":\\"px\\",\\"line_height_subheading_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]}],\\"styling\\":{\\"row_width\\":\\"\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"40\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\"}},{\\"row_order\\":\\"4\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-1 first\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/aromatheraphy-yellow.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/massage-oil.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/stone-massage.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"\\"}},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/face-massages.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/body-massage.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/massage-facial.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"ffffff_1.00\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"\\"}},{\\"column_order\\":\\"2\\",\\"grid_class\\":\\"col4-1\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/olive-spa.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/aromatheraphy-flower.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/waxing-1.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"\\"}},{\\"column_order\\":\\"3\\",\\"grid_class\\":\\"col4-1 last\\",\\"modules\\":[{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/aromatheraphy-2.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/massage-theraphy.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-overlay\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa-dev/files/2016/10/after-facial.jpg\\",\\"appearance_image\\":\\"|\\",\\"width_image\\":\\"290\\",\\"auto_fullwidth\\":\\"1\\",\\"height_image\\":\\"250\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"\\"}}],\\"gutter\\":\\"gutter-none\\",\\"column_alignment\\":\\"\\",\\"styling\\":{\\"row_width\\":\\"\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"ffffff_1.00\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"center\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"8\\",\\"padding_bottom_unit\\":\\"%\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\",\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"center\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"5\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first last\\",\\"modules\\":[],\\"styling\\":[]}],\\"styling\\":[]}]',
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 11,
  'post_date' => '2016-11-02 02:39:22',
  'post_date_gmt' => '2016-11-02 02:39:22',
  'post_content' => '',
  'post_title' => 'Contact',
  'post_excerpt' => '',
  'post_name' => 'contact',
  'post_modified' => '2017-06-22 20:34:49',
  'post_modified_gmt' => '2017-06-22 20:34:49',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?page_id=11',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
    'page_layout' => 'sidebar-none',
    'content_width' => 'full_width',
    'hide_page_title' => 'yes',
    'builder_switch_frontend' => '0',
    '_themify_builder_settings_json' => '[{\\"row_order\\":\\"0\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first\\",\\"modules\\":[{\\"mod_name\\":\\"maps-pro\\",\\"mod_settings\\":{\\"map_display_type\\":\\"dynamic\\",\\"w_map\\":\\"100\\",\\"unit_w\\":\\"%\\",\\"w_map_static\\":\\"500\\",\\"h_map\\":\\"645\\",\\"unit_h\\":\\"px\\",\\"type_map\\":\\"ROADMAP\\",\\"style_map\\":\\"greyscale\\",\\"scrollwheel_map\\":\\"disable\\",\\"draggable_map\\":\\"enable\\",\\"disable_map_ui\\":\\"no\\",\\"map_link\\":\\"|\\",\\"zoom_map\\":\\"17\\",\\"map_center\\":\\"1 Yonge Street,\\\\nToronto, ON\\\\nCanada\\",\\"markers\\":[{\\"address\\":\\"1 Yonge Street,\\\\nToronto, ON\\\\nCanada\\",\\"title\\":\\"Yonge St, Toronto ON\\",\\"image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/map-pin.png\\"}],\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"styling\\":[]}],\\"styling\\":{\\"row_width\\":\\"fullwidth\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\"}},{\\"row_order\\":\\"1\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col4-2 first\\",\\"modules\\":[{\\"mod_name\\":\\"plain-text\\",\\"mod_settings\\":{\\"plain_text\\":\\"<h2>Contact Form<\\\\/h2>\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"ffffff\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"center\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"image\\",\\"mod_settings\\":{\\"style_image\\":\\"image-center\\",\\"url_image\\":\\"https://themify.me/demo/themes/ultra-spa\\\\/files\\\\/2016\\\\/11\\\\/title-white-divider.png\\",\\"appearance_image\\":\\"|\\",\\"auto_fullwidth\\":\\"|\\",\\"param_image\\":\\"regular\\",\\"image_zoom_icon\\":\\"|\\",\\"lightbox_size_unit_width\\":\\"pixels\\",\\"lightbox_size_unit_height\\":\\"pixels\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top\\":\\"-2\\",\\"margin_top_unit\\":\\"em\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"3\\",\\"margin_bottom_unit\\":\\"em\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"|\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_title\\":\\"default\\",\\"font_size_title_unit\\":\\"px\\",\\"line_height_title_unit\\":\\"px\\",\\"font_family_caption\\":\\"default\\",\\"font_size_caption_unit\\":\\"px\\",\\"line_height_caption_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"contact\\",\\"mod_settings\\":{\\"layout_contact\\":\\"style1\\",\\"field_subject_active\\":\\"|\\",\\"field_captcha_active\\":\\"|\\",\\"field_sendcopy_active\\":\\"|\\",\\"font_family\\":\\"default\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"font_family_labels\\":\\"default\\",\\"font_size_labels_unit\\":\\"px\\",\\"font_family_inputs\\":\\"default\\",\\"font_size_inputs_unit\\":\\"px\\",\\"font_family_send\\":\\"default\\",\\"font_size_send_unit\\":\\"px\\",\\"font_family_success_message\\":\\"default\\",\\"text_align_success_message_left\\":\\"left\\",\\"text_align_success_message_center\\":\\"center\\",\\"text_align_success_message_right\\":\\"right\\",\\"text_align_success_message_justify\\":\\"justify\\",\\"padding_top_success_message_unit\\":\\"px\\",\\"padding_right_success_message_unit\\":\\"px\\",\\"padding_bottom_success_message_unit\\":\\"px\\",\\"padding_left_success_message_unit\\":\\"px\\",\\"margin_top_success_message_unit\\":\\"px\\",\\"margin_right_success_message_unit\\":\\"px\\",\\"margin_bottom_success_message_unit\\":\\"px\\",\\"margin_left_success_message_unit\\":\\"px\\",\\"font_family_error_message\\":\\"default\\",\\"text_align_error_message_left\\":\\"left\\",\\"text_align_error_message_center\\":\\"center\\",\\"text_align_error_message_right\\":\\"right\\",\\"text_align_error_message_justify\\":\\"justify\\",\\"padding_top_error_message_unit\\":\\"px\\",\\"padding_right_error_message_unit\\":\\"px\\",\\"padding_bottom_error_message_unit\\":\\"px\\",\\"padding_left_error_message_unit\\":\\"px\\",\\"margin_top_error_message_unit\\":\\"px\\",\\"margin_right_error_message_unit\\":\\"px\\",\\"margin_bottom_error_message_unit\\":\\"px\\",\\"margin_left_error_message_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"grid_width\\":\\"55.8175\\",\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"82d3da_1.00\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"50\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"50\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"50\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"50\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"\\"}},{\\"column_order\\":\\"1\\",\\"grid_class\\":\\"col4-2 last\\",\\"modules\\":[{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<p>128 Main Street <br \\\\/>Toronto, ON <br \\\\/>M1E 2G6<\\\\/p>\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_h1\\":\\"default\\",\\"font_size_h1_unit\\":\\"px\\",\\"line_height_h1_unit\\":\\"px\\",\\"font_family_h2\\":\\"default\\",\\"font_size_h2_unit\\":\\"px\\",\\"line_height_h2_unit\\":\\"px\\",\\"font_family_h3\\":\\"default\\",\\"font_size_h3_unit\\":\\"px\\",\\"line_height_h3_unit\\":\\"px\\",\\"font_family_h4\\":\\"default\\",\\"font_size_h4_unit\\":\\"px\\",\\"line_height_h4_unit\\":\\"px\\",\\"font_family_h5\\":\\"default\\",\\"font_size_h5_unit\\":\\"px\\",\\"line_height_h5_unit\\":\\"px\\",\\"font_family_h6\\":\\"default\\",\\"font_size_h6_unit\\":\\"px\\",\\"line_height_h6_unit\\":\\"px\\",\\"breakpoint_mobile\\":{\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"0\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(0deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(0deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(0deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(0deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(0deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"|\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"|\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_h1\\":\\"default\\",\\"font_size_h1_unit\\":\\"px\\",\\"line_height_h1_unit\\":\\"px\\",\\"font_family_h2\\":\\"default\\",\\"font_size_h2_unit\\":\\"px\\",\\"line_height_h2_unit\\":\\"px\\",\\"font_family_h3\\":\\"default\\",\\"font_size_h3_unit\\":\\"px\\",\\"line_height_h3_unit\\":\\"px\\",\\"font_family_h4\\":\\"default\\",\\"font_size_h4_unit\\":\\"px\\",\\"line_height_h4_unit\\":\\"px\\",\\"font_family_h5\\":\\"default\\",\\"font_size_h5_unit\\":\\"px\\",\\"line_height_h5_unit\\":\\"px\\",\\"font_family_h6\\":\\"default\\",\\"font_size_h6_unit\\":\\"px\\",\\"line_height_h6_unit\\":\\"px\\"}}},{\\"mod_name\\":\\"divider\\",\\"mod_settings\\":{\\"style_divider\\":\\"solid\\",\\"stroke_w_divider\\":\\"1\\",\\"color_divider\\":\\"dddddd_1.00\\",\\"divider_type\\":\\"fullwidth\\",\\"divider_width\\":\\"200\\",\\"divider_align\\":\\"left\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<h3>416-238-3368<\\\\/h3>\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"f1a4a4\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_h1\\":\\"default\\",\\"font_size_h1_unit\\":\\"px\\",\\"line_height_h1_unit\\":\\"px\\",\\"font_family_h2\\":\\"default\\",\\"font_size_h2_unit\\":\\"px\\",\\"line_height_h2_unit\\":\\"px\\",\\"font_family_h3\\":\\"default\\",\\"font_size_h3_unit\\":\\"px\\",\\"line_height_h3_unit\\":\\"px\\",\\"font_family_h4\\":\\"default\\",\\"font_size_h4_unit\\":\\"px\\",\\"line_height_h4_unit\\":\\"px\\",\\"font_family_h5\\":\\"default\\",\\"font_size_h5_unit\\":\\"px\\",\\"line_height_h5_unit\\":\\"px\\",\\"font_family_h6\\":\\"default\\",\\"font_size_h6_unit\\":\\"px\\",\\"line_height_h6_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"divider\\",\\"mod_settings\\":{\\"style_divider\\":\\"solid\\",\\"stroke_w_divider\\":\\"1\\",\\"color_divider\\":\\"dddddd_1.00\\",\\"divider_type\\":\\"fullwidth\\",\\"divider_width\\":\\"200\\",\\"divider_align\\":\\"left\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"text\\",\\"mod_settings\\":{\\"content_text\\":\\"<p>[themify_list style=\\\\\\\\\\\\\\"location-hours\\\\\\\\\\\\\\"]<\\\\/p>\\\\n<ul>\\\\n<li><strong>Mon - Fri<\\\\/strong> <em>11:00am - 8:00pm<\\\\/em><\\\\/li>\\\\n<li><strong>Sat<\\\\/strong> <em>9:00am - 8:00pm<\\\\/em><\\\\/li>\\\\n<li><strong>Sun<\\\\/strong> <em>9:00am - 5:00pm<\\\\/em><\\\\/li>\\\\n<\\\\/ul>\\\\n<p>[\\\\/themify_list]<\\\\/p>\\\\n\\",\\"background_image-type\\":\\"image\\",\\"background_image-type_image\\":\\"image\\",\\"background_image-type_gradient\\":\\"gradient\\",\\"background_image-gradient-type\\":\\"linear\\",\\"background_image-gradient-angle\\":\\"180\\",\\"background_image-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_image-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\n\\",\\"font_family\\":\\"default\\",\\"font_size_unit\\":\\"px\\",\\"line_height_unit\\":\\"px\\",\\"text_align_left\\":\\"left\\",\\"text_align_center\\":\\"center\\",\\"text_align_right\\":\\"right\\",\\"text_align_justify\\":\\"justify\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"checkbox_padding_apply_all_padding\\":\\"padding\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left_unit\\":\\"px\\",\\"checkbox_margin_apply_all\\":\\"margin\\",\\"checkbox_margin_apply_all_margin\\":\\"margin\\",\\"border_top_style\\":\\"solid\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"checkbox_border_apply_all_border\\":\\"border\\",\\"font_family_h1\\":\\"default\\",\\"font_size_h1_unit\\":\\"px\\",\\"line_height_h1_unit\\":\\"px\\",\\"font_family_h2\\":\\"default\\",\\"font_size_h2_unit\\":\\"px\\",\\"line_height_h2_unit\\":\\"px\\",\\"font_family_h3\\":\\"default\\",\\"font_size_h3_unit\\":\\"px\\",\\"line_height_h3_unit\\":\\"px\\",\\"font_family_h4\\":\\"default\\",\\"font_size_h4_unit\\":\\"px\\",\\"line_height_h4_unit\\":\\"px\\",\\"font_family_h5\\":\\"default\\",\\"font_size_h5_unit\\":\\"px\\",\\"line_height_h5_unit\\":\\"px\\",\\"font_family_h6\\":\\"default\\",\\"font_size_h6_unit\\":\\"px\\",\\"line_height_h6_unit\\":\\"px\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}},{\\"mod_name\\":\\"divider\\",\\"mod_settings\\":{\\"style_divider\\":\\"solid\\",\\"stroke_w_divider\\":\\"1\\",\\"color_divider\\":\\"dddddd_1.00\\",\\"divider_type\\":\\"fullwidth\\",\\"divider_width\\":\\"200\\",\\"divider_align\\":\\"left\\",\\"custom_parallax_scroll_reverse\\":\\"|\\",\\"custom_parallax_scroll_reverse_reverse\\":\\"reverse\\",\\"custom_parallax_scroll_fade\\":\\"|\\",\\"custom_parallax_scroll_fade_fade\\":\\"fade\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_desktop_show\\":\\"show\\",\\"visibility_desktop_hide\\":\\"hide\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_tablet_show\\":\\"show\\",\\"visibility_tablet_hide\\":\\"hide\\",\\"visibility_mobile\\":\\"show\\",\\"visibility_mobile_show\\":\\"show\\",\\"visibility_mobile_hide\\":\\"hide\\"}}],\\"grid_width\\":\\"40.9828\\",\\"styling\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"3\\",\\"padding_left_unit\\":\\"%\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"custom_css_column\\":\\"contact-info\\",\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"center\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"custom_css_column\\":\\"\\"}}}],\\"column_alignment\\":\\"col_align_middle\\",\\"styling\\":{\\"row_width\\":\\"\\",\\"row_height\\":\\"\\",\\"custom_css_row\\":\\"\\",\\"row_anchor\\":\\"\\",\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"6\\",\\"padding_bottom_unit\\":\\"%\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"margin_top\\":\\"-100\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\",\\"visibility_desktop\\":\\"show\\",\\"visibility_tablet\\":\\"show\\",\\"visibility_mobile\\":\\"show\\",\\"animation_effect\\":\\"\\",\\"animation_effect_delay\\":\\"\\",\\"animation_effect_repeat\\":\\"\\",\\"custom_parallax_scroll_speed\\":\\"\\",\\"custom_parallax_scroll_zindex\\":\\"\\",\\"breakpoint_tablet_landscape\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"30\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\"},\\"breakpoint_mobile\\":{\\"background_type\\":\\"image\\",\\"background_slider\\":\\"\\",\\"background_slider_size\\":\\"\\",\\"background_slider_mode\\":\\"\\",\\"background_video\\":\\"\\",\\"background_image\\":\\"\\",\\"background_gradient-gradient-type\\":\\"linear\\",\\"background_gradient-gradient-angle\\":\\"180\\",\\"background_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"background_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"background_repeat\\":\\"\\",\\"background_position\\":\\"\\",\\"background_color\\":\\"\\",\\"cover_color-type\\":\\"color\\",\\"cover_color\\":\\"\\",\\"cover_gradient-gradient-type\\":\\"linear\\",\\"cover_gradient-gradient-angle\\":\\"180\\",\\"cover_gradient-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"cover_color_hover-type\\":\\"hover_color\\",\\"cover_color_hover\\":\\"\\",\\"cover_gradient_hover-gradient-type\\":\\"linear\\",\\"cover_gradient_hover-gradient-angle\\":\\"180\\",\\"cover_gradient_hover-gradient\\":\\"0% rgb(0, 0, 0)|100% rgb(255, 255, 255)\\",\\"cover_gradient_hover-css\\":\\"background-image: -moz-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -webkit-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -o-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: -ms-linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\nbackground-image: linear-gradient(180deg,rgb(0, 0, 0) 0%, rgb(255, 255, 255) 100%);\\\\r\\\\n\\",\\"font_family\\":\\"default\\",\\"font_color\\":\\"\\",\\"font_size\\":\\"\\",\\"font_size_unit\\":\\"px\\",\\"line_height\\":\\"\\",\\"line_height_unit\\":\\"px\\",\\"text_align\\":\\"\\",\\"link_color\\":\\"\\",\\"text_decoration\\":\\"\\",\\"padding_top\\":\\"\\",\\"padding_top_unit\\":\\"px\\",\\"padding_right\\":\\"\\",\\"padding_right_unit\\":\\"px\\",\\"padding_bottom\\":\\"\\",\\"padding_bottom_unit\\":\\"px\\",\\"padding_left\\":\\"\\",\\"padding_left_unit\\":\\"px\\",\\"checkbox_padding_apply_all\\":\\"padding\\",\\"margin_top\\":\\"50\\",\\"margin_top_unit\\":\\"px\\",\\"margin_right\\":\\"\\",\\"margin_right_unit\\":\\"px\\",\\"margin_bottom\\":\\"\\",\\"margin_bottom_unit\\":\\"px\\",\\"margin_left\\":\\"\\",\\"margin_left_unit\\":\\"px\\",\\"border_top_color\\":\\"\\",\\"border_top_width\\":\\"\\",\\"border_top_style\\":\\"solid\\",\\"border_right_color\\":\\"\\",\\"border_right_width\\":\\"\\",\\"border_right_style\\":\\"solid\\",\\"border_bottom_color\\":\\"\\",\\"border_bottom_width\\":\\"\\",\\"border_bottom_style\\":\\"solid\\",\\"border_left_color\\":\\"\\",\\"border_left_width\\":\\"\\",\\"border_left_style\\":\\"solid\\",\\"checkbox_border_apply_all\\":\\"border\\"}}},{\\"row_order\\":\\"2\\",\\"cols\\":[{\\"column_order\\":\\"0\\",\\"grid_class\\":\\"col-full first last\\",\\"modules\\":[],\\"styling\\":[]}],\\"styling\\":[]}]',
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 19,
  'post_date' => '2016-11-02 02:48:57',
  'post_date_gmt' => '2016-11-02 02:48:57',
  'post_content' => '',
  'post_title' => 'Shop',
  'post_excerpt' => '',
  'post_name' => 'shop',
  'post_modified' => '2016-11-09 03:09:51',
  'post_modified_gmt' => '2016-11-09 03:09:51',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/shop/',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
    'page_layout' => 'sidebar-none',
    'builder_switch_frontend' => '0',
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 20,
  'post_date' => '2016-11-02 02:48:57',
  'post_date_gmt' => '2016-11-02 02:48:57',
  'post_content' => '[woocommerce_cart]',
  'post_title' => 'Cart',
  'post_excerpt' => '',
  'post_name' => 'cart',
  'post_modified' => '2016-11-02 02:48:57',
  'post_modified_gmt' => '2016-11-02 02:48:57',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/cart/',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 21,
  'post_date' => '2016-11-02 02:48:57',
  'post_date_gmt' => '2016-11-02 02:48:57',
  'post_content' => '[woocommerce_checkout]',
  'post_title' => 'Checkout',
  'post_excerpt' => '',
  'post_name' => 'checkout',
  'post_modified' => '2016-11-02 02:48:57',
  'post_modified_gmt' => '2016-11-02 02:48:57',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/checkout/',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 22,
  'post_date' => '2016-11-02 02:48:57',
  'post_date_gmt' => '2016-11-02 02:48:57',
  'post_content' => '[woocommerce_my_account]',
  'post_title' => 'My Account',
  'post_excerpt' => '',
  'post_name' => 'my-account',
  'post_modified' => '2016-11-02 02:48:57',
  'post_modified_gmt' => '2016-11-02 02:48:57',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/my-account/',
  'menu_order' => 0,
  'post_type' => 'page',
  'meta_input' => 
  array (
  ),
  'tax_input' => 
  array (
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 42,
  'post_date' => '2016-10-14 15:42:21',
  'post_date_gmt' => '2016-10-14 15:42:21',
  'post_content' => '<div>

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

</div>',
  'post_title' => 'Kannel N6',
  'post_excerpt' => 'Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.',
  'post_name' => 'kannel-n6',
  'post_modified' => '2016-11-07 06:30:03',
  'post_modified_gmt' => '2016-11-07 06:30:03',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=42',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500203:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '45',
    '_sale_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_thumbnail_id' => '55',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/10/kennel-n6.jpg',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'soap',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 44,
  'post_date' => '2016-10-15 16:12:17',
  'post_date_gmt' => '2016-10-15 16:12:17',
  'post_content' => '<div>

Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem.

</div>',
  'post_title' => 'Shaving Foam',
  'post_excerpt' => 'Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat.',
  'post_name' => 'shaving-foam',
  'post_modified' => '2016-11-07 06:29:09',
  'post_modified_gmt' => '2016-11-07 06:29:09',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=44',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500149:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
    '_thumbnail_id' => '56',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/10/shave-tube.jpg',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'shaving',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 47,
  'post_date' => '2016-10-16 16:20:26',
  'post_date_gmt' => '2016-10-16 16:20:26',
  'post_content' => 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  'post_title' => 'Flava Shampoo',
  'post_excerpt' => '<div>

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

</div>',
  'post_name' => 'flava-shampoo',
  'post_modified' => '2016-11-07 06:29:02',
  'post_modified_gmt' => '2016-11-07 06:29:02',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=47',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500142:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '18',
    '_price' => '18',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_thumbnail_id' => '54',
    '_wc_review_count' => '0',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/10/flava-shampoo-1.jpg',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'shampoo',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 49,
  'post_date' => '2016-10-23 16:22:33',
  'post_date_gmt' => '2016-10-23 16:22:33',
  'post_content' => '<div>

At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus.

</div>',
  'post_title' => 'Bath Salt',
  'post_excerpt' => 'Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem.',
  'post_name' => 'bath-salt',
  'post_modified' => '2016-11-07 06:28:29',
  'post_modified_gmt' => '2016-11-07 06:28:29',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=49',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500109:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '25',
    '_price' => '25',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_thumbnail_id' => '52',
    '_wc_review_count' => '0',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/10/olive-soap.jpg',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'soap',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 57,
  'post_date' => '2016-09-04 07:39:52',
  'post_date_gmt' => '2016-09-04 07:39:52',
  'post_content' => 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.',
  'post_title' => 'Night Cream',
  'post_excerpt' => 'At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae.',
  'post_name' => 'night-cream',
  'post_modified' => '2016-11-07 06:31:23',
  'post_modified_gmt' => '2016-11-07 06:31:23',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=57',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500283:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '58',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/facial-cream-2.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '25',
    '_price' => '25',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 59,
  'post_date' => '2016-09-05 07:43:38',
  'post_date_gmt' => '2016-09-05 07:43:38',
  'post_content' => 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.',
  'post_title' => 'Apple Shampoo',
  'post_excerpt' => 'Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?',
  'post_name' => 'apple-shampoo',
  'post_modified' => '2016-11-07 06:31:15',
  'post_modified_gmt' => '2016-11-07 06:31:15',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=59',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500275:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '60',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/apple-shampoo.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '45',
    '_price' => '45',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'shampoo',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 62,
  'post_date' => '2016-09-07 07:46:15',
  'post_date_gmt' => '2016-09-07 07:46:15',
  'post_content' => 'At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio.',
  'post_title' => 'Sunblock Cream',
  'post_excerpt' => 'Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat',
  'post_name' => 'sunblock-cream',
  'post_modified' => '2016-11-07 06:30:57',
  'post_modified_gmt' => '2016-11-07 06:30:57',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=62',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500257:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '63',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/sunblock.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 64,
  'post_date' => '2016-09-06 07:48:56',
  'post_date_gmt' => '2016-09-06 07:48:56',
  'post_content' => 'Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.',
  'post_title' => 'Whitening Skin Refill Soap',
  'post_excerpt' => 'Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.',
  'post_name' => 'whitening-skin-refill-soap',
  'post_modified' => '2016-11-07 06:31:05',
  'post_modified_gmt' => '2016-11-07 06:31:05',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=64',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500265:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '65',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/skin-whitening-pills.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '78',
    '_price' => '78',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'soap',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 66,
  'post_date' => '2016-09-09 07:58:29',
  'post_date_gmt' => '2016-09-09 07:58:29',
  'post_content' => 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  'post_title' => 'White Pearl Soap',
  'post_excerpt' => 'Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  'post_name' => 'white-pearl-soap',
  'post_modified' => '2016-11-07 06:30:48',
  'post_modified_gmt' => '2016-11-07 06:30:48',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=66',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500248:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '67',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/white-pearl-soap.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '55',
    '_sale_price' => '45',
    '_price' => '45',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'soap',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 68,
  'post_date' => '2016-09-11 08:01:21',
  'post_date_gmt' => '2016-09-11 08:01:21',
  'post_content' => 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo.',
  'post_title' => 'Waxing Cream',
  'post_excerpt' => 'Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.',
  'post_name' => 'waxing-cream',
  'post_modified' => '2016-11-07 06:30:40',
  'post_modified_gmt' => '2016-11-07 06:30:40',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=68',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500240:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '69',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/waxing-cream.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'waxing',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 70,
  'post_date' => '2016-10-17 08:04:35',
  'post_date_gmt' => '2016-10-17 08:04:35',
  'post_content' => 'At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio.',
  'post_title' => 'Aromatherapy Essence',
  'post_excerpt' => 'Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat.',
  'post_name' => 'aromatherapy-essence',
  'post_modified' => '2016-11-07 06:28:54',
  'post_modified_gmt' => '2016-11-07 06:28:54',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=70',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500134:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '71',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/aromatheraphy-oil.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '20',
    '_price' => '20',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'aromatherapy',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 72,
  'post_date' => '2016-09-18 08:42:49',
  'post_date_gmt' => '2016-09-18 08:42:49',
  'post_content' => 'Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem.',
  'post_title' => 'Night Cream',
  'post_excerpt' => 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.',
  'post_name' => 'night-cream-2',
  'post_modified' => '2016-11-07 06:30:32',
  'post_modified_gmt' => '2016-11-07 06:30:32',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=72',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500232:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '73',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/whitening-cream.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '16',
    '_price' => '16',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 74,
  'post_date' => '2016-09-22 09:01:00',
  'post_date_gmt' => '2016-09-22 09:01:00',
  'post_content' => 'At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga.',
  'post_title' => 'Avocado Body Cream',
  'post_excerpt' => 'Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat.',
  'post_name' => 'avocado-body-cream',
  'post_modified' => '2016-11-07 06:30:26',
  'post_modified_gmt' => '2016-11-07 06:30:26',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=74',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500226:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '75',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/avocado-cream.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 76,
  'post_date' => '2016-09-26 09:06:51',
  'post_date_gmt' => '2016-09-26 09:06:51',
  'post_content' => 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt.',
  'post_title' => 'Red Nail Paint',
  'post_excerpt' => 'Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur. Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur.',
  'post_name' => 'red-nail-paint',
  'post_modified' => '2016-11-07 06:30:19',
  'post_modified_gmt' => '2016-11-07 06:30:19',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=76',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500219:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '77',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/nail-paint.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '15',
    '_price' => '15',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'nail-paint',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 79,
  'post_date' => '2016-10-04 09:19:28',
  'post_date_gmt' => '2016-10-04 09:19:28',
  'post_content' => 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.',
  'post_title' => 'Cosmetic Brush Set',
  'post_excerpt' => 'Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.',
  'post_name' => 'cosmetic-brush-set',
  'post_modified' => '2016-11-07 06:30:11',
  'post_modified_gmt' => '2016-11-07 06:30:11',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=79',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500211:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '80',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/cosmetic-brush-set.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '12',
    '_price' => '12',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cosmetic',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 81,
  'post_date' => '2016-10-22 15:44:37',
  'post_date_gmt' => '2016-10-22 15:44:37',
  'post_content' => '<div>

Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus.

</div>',
  'post_title' => 'Skin Supplement',
  'post_excerpt' => 'Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus.',
  'post_name' => 'skin-supplement',
  'post_modified' => '2016-11-07 06:28:40',
  'post_modified_gmt' => '2016-11-07 06:28:40',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=81',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500120:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '82',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/skin-suplement.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '25',
    '_price' => '25',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'supplement',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 83,
  'post_date' => '2016-10-14 15:48:16',
  'post_date_gmt' => '2016-10-14 15:48:16',
  'post_content' => 'Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam.',
  'post_title' => 'Aloe Vera Cream',
  'post_excerpt' => 'Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit.',
  'post_name' => 'aloe-vera-cream',
  'post_modified' => '2016-11-07 06:29:17',
  'post_modified_gmt' => '2016-11-07 06:29:17',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=83',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500157:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '84',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/alole-vera.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '20',
    '_price' => '20',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 85,
  'post_date' => '2016-10-26 16:03:14',
  'post_date_gmt' => '2016-10-26 16:03:14',
  'post_content' => '<div>

Adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur.

</div>',
  'post_title' => 'Anti Acne Cream Package',
  'post_excerpt' => 'Vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus.',
  'post_name' => 'anti-acne-cream-package',
  'post_modified' => '2016-11-07 06:28:20',
  'post_modified_gmt' => '2016-11-07 06:28:20',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=85',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500100:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '86',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/anti-acne-cream-package.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '150',
    '_price' => '150',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'anti-acne',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 87,
  'post_date' => '2016-11-01 16:20:05',
  'post_date_gmt' => '2016-11-01 16:20:05',
  'post_content' => '<div>

Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil.

</div>',
  'post_title' => 'Cosmetic Set',
  'post_excerpt' => 'Similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et aut officiis debitis aut rerum.',
  'post_name' => 'cosmetic-set',
  'post_modified' => '2016-11-07 06:28:12',
  'post_modified_gmt' => '2016-11-07 06:28:12',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=87',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500092:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '88',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/pink-cosmetic.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '70',
    '_price' => '70',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_cat' => 'cosmetic',
    'product_tag' => 'cosmetic',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 89,
  'post_date' => '2016-08-04 16:52:56',
  'post_date_gmt' => '2016-08-04 16:52:56',
  'post_content' => '<div>

Deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum.

</div>',
  'post_title' => 'After Bath Skin Treatment Set',
  'post_excerpt' => 'Vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga. Vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum.',
  'post_name' => 'bath-skin-treatment-set',
  'post_modified' => '2016-11-07 06:31:32',
  'post_modified_gmt' => '2016-11-07 06:31:32',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?post_type=product&#038;p=89',
  'menu_order' => 0,
  'post_type' => 'product',
  'meta_input' => 
  array (
    '_edit_lock' => '1478500292:172',
    '_edit_last' => '172',
    '_themify_builder_settings_json' => '[{"row_order":"0","gutter":"gutter-default","equal_column_height":"","column_alignment":"col_align_top","cols":[{"column_order":"0","grid_class":"col-full first last","grid_width":"","modules":[],"styling":[]}],"styling":[]}]',
    '_visibility' => 'visible',
    '_stock_status' => 'instock',
    'builder_switch_frontend' => '0',
    '_thumbnail_id' => '90',
    'post_image' => 'https://themify.me/demo/themes/ultra-spa/files/2016/11/afterbath.jpg',
    'total_sales' => '0',
    '_downloadable' => 'no',
    '_virtual' => 'no',
    '_featured' => 'no',
    '_product_attributes' => 
    array (
    ),
    '_regular_price' => '35',
    '_price' => '35',
    '_manage_stock' => 'no',
    '_backorders' => 'no',
    '_upsell_ids' => 
    array (
    ),
    '_crosssell_ids' => 
    array (
    ),
    '_product_version' => '2.6.4',
    '_yoast_wpseo_content_score' => '30',
    '_wc_rating_count' => 
    array (
    ),
    '_wc_average_rating' => '0',
    '_wc_review_count' => '0',
  ),
  'tax_input' => 
  array (
    'product_type' => 'simple',
    'product_tag' => 'cream',
  ),
  'has_thumbnail' => true,
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 18,
  'post_date' => '2016-11-02 02:42:27',
  'post_date_gmt' => '2016-11-02 02:42:27',
  'post_content' => ' ',
  'post_title' => '',
  'post_excerpt' => '',
  'post_name' => '18',
  'post_modified' => '2016-11-02 02:50:50',
  'post_modified_gmt' => '2016-11-02 02:50:50',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?p=18',
  'menu_order' => 1,
  'post_type' => 'nav_menu_item',
  'meta_input' => 
  array (
    '_menu_item_type' => 'post_type',
    '_menu_item_menu_item_parent' => '0',
    '_menu_item_object_id' => '6',
    '_menu_item_object' => 'page',
    '_menu_item_classes' => 
    array (
      0 => '',
    ),
  ),
  'tax_input' => 
  array (
    'nav_menu' => 'main-navigation',
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 17,
  'post_date' => '2016-11-02 02:42:27',
  'post_date_gmt' => '2016-11-02 02:42:27',
  'post_content' => ' ',
  'post_title' => '',
  'post_excerpt' => '',
  'post_name' => '17',
  'post_modified' => '2016-11-02 02:50:50',
  'post_modified_gmt' => '2016-11-02 02:50:50',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?p=17',
  'menu_order' => 2,
  'post_type' => 'nav_menu_item',
  'meta_input' => 
  array (
    '_menu_item_type' => 'post_type',
    '_menu_item_menu_item_parent' => '0',
    '_menu_item_object_id' => '8',
    '_menu_item_object' => 'page',
    '_menu_item_classes' => 
    array (
      0 => '',
    ),
  ),
  'tax_input' => 
  array (
    'nav_menu' => 'main-navigation',
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 16,
  'post_date' => '2016-11-02 02:42:27',
  'post_date_gmt' => '2016-11-02 02:42:27',
  'post_content' => ' ',
  'post_title' => '',
  'post_excerpt' => '',
  'post_name' => '16',
  'post_modified' => '2016-11-02 02:50:50',
  'post_modified_gmt' => '2016-11-02 02:50:50',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?p=16',
  'menu_order' => 3,
  'post_type' => 'nav_menu_item',
  'meta_input' => 
  array (
    '_menu_item_type' => 'post_type',
    '_menu_item_menu_item_parent' => '0',
    '_menu_item_object_id' => '10',
    '_menu_item_object' => 'page',
    '_menu_item_classes' => 
    array (
      0 => '',
    ),
  ),
  'tax_input' => 
  array (
    'nav_menu' => 'main-navigation',
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 24,
  'post_date' => '2016-11-02 02:50:50',
  'post_date_gmt' => '2016-11-02 02:50:50',
  'post_content' => ' ',
  'post_title' => '',
  'post_excerpt' => '',
  'post_name' => '24',
  'post_modified' => '2016-11-02 02:50:50',
  'post_modified_gmt' => '2016-11-02 02:50:50',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?p=24',
  'menu_order' => 4,
  'post_type' => 'nav_menu_item',
  'meta_input' => 
  array (
    '_menu_item_type' => 'post_type',
    '_menu_item_menu_item_parent' => '0',
    '_menu_item_object_id' => '19',
    '_menu_item_object' => 'page',
    '_menu_item_classes' => 
    array (
      0 => '',
    ),
  ),
  'tax_input' => 
  array (
    'nav_menu' => 'main-navigation',
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}

$post = array (
  'ID' => 15,
  'post_date' => '2016-11-02 02:42:27',
  'post_date_gmt' => '2016-11-02 02:42:27',
  'post_content' => '',
  'post_title' => 'Contact Us',
  'post_excerpt' => '',
  'post_name' => '15',
  'post_modified' => '2016-11-02 02:50:50',
  'post_modified_gmt' => '2016-11-02 02:50:50',
  'post_content_filtered' => '',
  'post_parent' => 0,
  'guid' => 'https://themify.me/demo/themes/ultra-spa/?p=15',
  'menu_order' => 5,
  'post_type' => 'nav_menu_item',
  'meta_input' => 
  array (
    '_menu_item_type' => 'post_type',
    '_menu_item_menu_item_parent' => '0',
    '_menu_item_object_id' => '11',
    '_menu_item_object' => 'page',
    '_menu_item_classes' => 
    array (
      0 => '',
    ),
  ),
  'tax_input' => 
  array (
    'nav_menu' => 'main-navigation',
  ),
);

if( ERASEDEMO ) {
	themify_undo_import_post( $post );
} else {
	themify_import_post( $post );
}


function themify_import_get_term_id_from_slug( $slug ) {
	$menu = get_term_by( "slug", $slug, "nav_menu" );
	return is_wp_error( $menu ) ? 0 : (int) $menu->term_id;
}

	$widgets = get_option( "widget_woocommerce_products" );
$widgets[1002] = array (
  'title' => 'Recent Products',
  'number' => 5,
  'show' => '',
  'orderby' => 'date',
  'order' => 'desc',
  'hide_free' => 0,
  'show_hidden' => 0,
);
update_option( "widget_woocommerce_products", $widgets );

$widgets = get_option( "widget_woocommerce_price_filter" );
$widgets[1003] = array (
  'title' => 'Filter by price',
);
update_option( "widget_woocommerce_price_filter", $widgets );

$widgets = get_option( "widget_woocommerce_product_tag_cloud" );
$widgets[1004] = array (
  'title' => 'Product Tags',
);
update_option( "widget_woocommerce_product_tag_cloud", $widgets );



$sidebars_widgets = array (
  'sidebar-main' => 
  array (
    0 => 'woocommerce_products-1002',
    1 => 'woocommerce_price_filter-1003',
    2 => 'woocommerce_product_tag_cloud-1004',
  ),
); 
update_option( "sidebars_widgets", $sidebars_widgets );

$menu_locations = array();
$menu = get_terms( "nav_menu", array( "slug" => "main-navigation" ) );
if( is_array( $menu ) && ! empty( $menu ) ) $menu_locations["main-nav"] = $menu[0]->term_id;
set_theme_mod( "nav_menu_locations", $menu_locations );


$homepage = get_posts( array( 'name' => 'home', 'post_type' => 'page' ) );
			if( is_array( $homepage ) && ! empty( $homepage ) ) {
				update_option( 'show_on_front', 'page' );
				update_option( 'page_on_front', $homepage[0]->ID );
			}
			
	ob_start(); ?>a:281:{s:15:"setting-favicon";s:0:"";s:23:"setting-custom_feed_url";s:0:"";s:19:"setting-header_html";s:0:"";s:19:"setting-footer_html";s:0:"";s:23:"setting-search_settings";s:0:"";s:16:"setting-page_404";s:1:"0";s:21:"setting-feed_settings";s:0:"";s:21:"setting-webfonts_list";s:11:"recommended";s:24:"setting-webfonts_subsets";s:0:"";s:22:"setting-default_layout";s:8:"sidebar1";s:27:"setting-default_post_layout";s:9:"list-post";s:27:"setting-post_content_layout";s:0:"";s:23:"setting-disable_masonry";s:3:"yes";s:19:"setting-post_gutter";s:6:"gutter";s:30:"setting-default_layout_display";s:7:"content";s:25:"setting-default_more_text";s:4:"More";s:21:"setting-index_orderby";s:4:"date";s:19:"setting-index_order";s:4:"DESC";s:26:"setting-default_post_title";s:0:"";s:33:"setting-default_unlink_post_title";s:0:"";s:25:"setting-default_post_meta";s:0:"";s:32:"setting-default_post_meta_author";s:0:"";s:34:"setting-default_post_meta_category";s:0:"";s:33:"setting-default_post_meta_comment";s:0:"";s:29:"setting-default_post_meta_tag";s:0:"";s:25:"setting-default_post_date";s:0:"";s:30:"setting-default_media_position";s:5:"above";s:26:"setting-default_post_image";s:0:"";s:33:"setting-default_unlink_post_image";s:0:"";s:31:"setting-image_post_feature_size";s:5:"blank";s:24:"setting-image_post_width";s:0:"";s:25:"setting-image_post_height";s:0:"";s:32:"setting-default_page_post_layout";s:8:"sidebar1";s:37:"setting-default_page_post_layout_type";s:7:"classic";s:31:"setting-default_page_post_title";s:0:"";s:38:"setting-default_page_unlink_post_title";s:0:"";s:30:"setting-default_page_post_meta";s:0:"";s:37:"setting-default_page_post_meta_author";s:0:"";s:39:"setting-default_page_post_meta_category";s:0:"";s:38:"setting-default_page_post_meta_comment";s:0:"";s:34:"setting-default_page_post_meta_tag";s:0:"";s:30:"setting-default_page_post_date";s:0:"";s:31:"setting-default_page_post_image";s:0:"";s:38:"setting-default_page_unlink_post_image";s:0:"";s:38:"setting-image_post_single_feature_size";s:5:"blank";s:31:"setting-image_post_single_width";s:0:"";s:32:"setting-image_post_single_height";s:0:"";s:27:"setting-default_page_layout";s:8:"sidebar1";s:23:"setting-hide_page_title";s:0:"";s:23:"setting-hide_page_image";s:0:"";s:33:"setting-page_featured_image_width";s:0:"";s:34:"setting-page_featured_image_height";s:0:"";s:38:"setting-default_portfolio_index_layout";s:12:"sidebar-none";s:43:"setting-default_portfolio_index_post_layout";s:5:"grid3";s:32:"setting-portfolio_content_layout";s:0:"";s:33:"setting-portfolio_disable_masonry";s:3:"yes";s:24:"setting-portfolio_gutter";s:6:"gutter";s:39:"setting-default_portfolio_index_display";s:4:"none";s:37:"setting-default_portfolio_index_title";s:0:"";s:49:"setting-default_portfolio_index_unlink_post_title";s:0:"";s:50:"setting-default_portfolio_index_post_meta_category";s:3:"yes";s:49:"setting-default_portfolio_index_unlink_post_image";s:3:"yes";s:48:"setting-default_portfolio_index_image_post_width";s:0:"";s:49:"setting-default_portfolio_index_image_post_height";s:0:"";s:54:"setting-default_portfolio_single_portfolio_layout_type";s:9:"fullwidth";s:38:"setting-default_portfolio_single_title";s:0:"";s:50:"setting-default_portfolio_single_unlink_post_title";s:0:"";s:51:"setting-default_portfolio_single_post_meta_category";s:0:"";s:50:"setting-default_portfolio_single_unlink_post_image";s:3:"yes";s:49:"setting-default_portfolio_single_image_post_width";s:0:"";s:50:"setting-default_portfolio_single_image_post_height";s:0:"";s:22:"themify_portfolio_slug";s:7:"project";s:19:"setting-shop_layout";s:8:"sidebar1";s:27:"setting-shop_archive_layout";s:12:"sidebar-none";s:31:"setting-product_disable_masonry";s:3:"yes";s:30:"setting-shop_products_per_page";s:0:"";s:34:"setting-product_archive_hide_title";s:0:"";s:34:"setting-product_archive_hide_price";s:0:"";s:34:"setting-product_archive_show_short";s:0:"";s:29:"setting-single_product_layout";s:8:"sidebar1";s:30:"setting-related_products_limit";s:1:"3";s:53:"setting-customizer_responsive_design_tablet_landscape";s:4:"1024";s:43:"setting-customizer_responsive_design_tablet";s:3:"768";s:43:"setting-customizer_responsive_design_mobile";s:3:"480";s:33:"setting-mobile_menu_trigger_point";s:4:"1200";s:24:"setting-gallery_lightbox";s:8:"lightbox";s:26:"setting-page_builder_cache";s:2:"on";s:27:"setting-script_minification";s:7:"disable";s:27:"setting-page_builder_expiry";s:1:"2";s:20:"setting-color_design";s:7:"default";s:19:"setting-font_design";s:7:"default";s:21:"setting-header_design";s:17:"header-horizontal";s:28:"setting-exclude_site_tagline";s:2:"on";s:27:"setting-exclude_search_form";s:2:"on";s:19:"setting-exclude_rss";s:2:"on";s:29:"setting-exclude_social_widget";s:2:"on";s:22:"setting-header_widgets";s:4:"none";s:21:"setting-footer_design";s:12:"footer-block";s:32:"setting-exclude_footer_site_logo";s:2:"on";s:38:"setting-exclude_footer_menu_navigation";s:2:"on";s:22:"setting-footer_widgets";s:17:"footerwidget-1col";s:30:"setting-footer_widget_position";s:0:"";s:27:"setting-imagefilter_options";s:0:"";s:33:"setting-imagefilter_options_hover";s:0:"";s:27:"setting-imagefilter_applyto";s:12:"featuredonly";s:25:"setting-page_loader_color";s:0:"";s:24:"setting-page_loader_icon";s:0:"";s:29:"setting-color_animation_speed";s:1:"5";s:20:"setting-color_stop_1";s:0:"";s:20:"setting-color_stop_2";s:0:"";s:20:"setting-color_stop_3";s:0:"";s:20:"setting-color_stop_4";s:0:"";s:20:"setting-color_stop_5";s:0:"";s:20:"setting-color_stop_6";s:0:"";s:20:"setting-color_stop_7";s:0:"";s:29:"setting-relationship_taxonomy";s:8:"category";s:37:"setting-relationship_taxonomy_entries";s:1:"3";s:45:"setting-relationship_taxonomy_display_content";s:4:"none";s:30:"setting-single_slider_autoplay";s:3:"off";s:27:"setting-single_slider_speed";s:6:"normal";s:28:"setting-single_slider_effect";s:5:"slide";s:28:"setting-single_slider_height";s:4:"auto";s:18:"setting-more_posts";s:8:"infinite";s:19:"setting-entries_nav";s:8:"numbered";s:24:"setting-footer_text_left";s:48:"<h3>VISIT US</h3>
128 Main Street, Toronto, ON.";s:25:"setting-footer_text_right";s:21:"<h3>416-238-3368</h3>";s:27:"setting-global_feature_size";s:5:"blank";s:22:"setting-link_icon_type";s:9:"font-icon";s:32:"setting-link_type_themify-link-0";s:10:"image-icon";s:33:"setting-link_title_themify-link-0";s:7:"Twitter";s:32:"setting-link_link_themify-link-0";s:0:"";s:31:"setting-link_img_themify-link-0";s:103:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/themify/img/social/twitter.png";s:32:"setting-link_type_themify-link-1";s:10:"image-icon";s:33:"setting-link_title_themify-link-1";s:8:"Facebook";s:32:"setting-link_link_themify-link-1";s:0:"";s:31:"setting-link_img_themify-link-1";s:104:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/themify/img/social/facebook.png";s:32:"setting-link_type_themify-link-2";s:10:"image-icon";s:33:"setting-link_title_themify-link-2";s:7:"Google+";s:32:"setting-link_link_themify-link-2";s:0:"";s:31:"setting-link_img_themify-link-2";s:107:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/themify/img/social/google-plus.png";s:32:"setting-link_type_themify-link-3";s:10:"image-icon";s:33:"setting-link_title_themify-link-3";s:7:"YouTube";s:32:"setting-link_link_themify-link-3";s:0:"";s:31:"setting-link_img_themify-link-3";s:103:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/themify/img/social/youtube.png";s:32:"setting-link_type_themify-link-4";s:10:"image-icon";s:33:"setting-link_title_themify-link-4";s:9:"Pinterest";s:32:"setting-link_link_themify-link-4";s:0:"";s:31:"setting-link_img_themify-link-4";s:105:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/themify/img/social/pinterest.png";s:32:"setting-link_type_themify-link-5";s:9:"font-icon";s:33:"setting-link_title_themify-link-5";s:7:"Twitter";s:32:"setting-link_link_themify-link-5";s:0:"";s:33:"setting-link_ficon_themify-link-5";s:10:"fa-twitter";s:35:"setting-link_ficolor_themify-link-5";s:0:"";s:37:"setting-link_fibgcolor_themify-link-5";s:0:"";s:32:"setting-link_type_themify-link-6";s:9:"font-icon";s:33:"setting-link_title_themify-link-6";s:8:"Facebook";s:32:"setting-link_link_themify-link-6";s:0:"";s:33:"setting-link_ficon_themify-link-6";s:11:"fa-facebook";s:35:"setting-link_ficolor_themify-link-6";s:0:"";s:37:"setting-link_fibgcolor_themify-link-6";s:0:"";s:32:"setting-link_type_themify-link-7";s:9:"font-icon";s:33:"setting-link_title_themify-link-7";s:7:"Google+";s:32:"setting-link_link_themify-link-7";s:0:"";s:33:"setting-link_ficon_themify-link-7";s:14:"fa-google-plus";s:35:"setting-link_ficolor_themify-link-7";s:0:"";s:37:"setting-link_fibgcolor_themify-link-7";s:0:"";s:32:"setting-link_type_themify-link-8";s:9:"font-icon";s:33:"setting-link_title_themify-link-8";s:7:"YouTube";s:32:"setting-link_link_themify-link-8";s:0:"";s:33:"setting-link_ficon_themify-link-8";s:10:"fa-youtube";s:35:"setting-link_ficolor_themify-link-8";s:0:"";s:37:"setting-link_fibgcolor_themify-link-8";s:0:"";s:32:"setting-link_type_themify-link-9";s:9:"font-icon";s:33:"setting-link_title_themify-link-9";s:9:"Pinterest";s:32:"setting-link_link_themify-link-9";s:0:"";s:33:"setting-link_ficon_themify-link-9";s:12:"fa-pinterest";s:35:"setting-link_ficolor_themify-link-9";s:0:"";s:37:"setting-link_fibgcolor_themify-link-9";s:0:"";s:22:"setting-link_field_ids";s:341:"{"themify-link-0":"themify-link-0","themify-link-1":"themify-link-1","themify-link-2":"themify-link-2","themify-link-3":"themify-link-3","themify-link-4":"themify-link-4","themify-link-5":"themify-link-5","themify-link-6":"themify-link-6","themify-link-7":"themify-link-7","themify-link-8":"themify-link-8","themify-link-9":"themify-link-9"}";s:23:"setting-link_field_hash";s:2:"10";s:30:"setting-page_builder_is_active";s:6:"enable";s:41:"setting-page_builder_animation_appearance";s:0:"";s:42:"setting-page_builder_animation_parallax_bg";s:0:"";s:46:"setting-page_builder_animation_parallax_scroll";s:6:"mobile";s:55:"setting-page_builder_responsive_design_tablet_landscape";s:4:"1024";s:45:"setting-page_builder_responsive_design_tablet";s:3:"768";s:45:"setting-page_builder_responsive_design_mobile";s:3:"680";s:23:"setting-hooks_field_ids";s:2:"[]";s:27:"setting-custom_panel-editor";s:7:"default";s:27:"setting-custom_panel-author";s:7:"default";s:32:"setting-custom_panel-contributor";s:7:"default";s:33:"setting-custom_panel-shop_manager";s:7:"default";s:25:"setting-customizer-editor";s:7:"default";s:25:"setting-customizer-author";s:7:"default";s:30:"setting-customizer-contributor";s:7:"default";s:31:"setting-customizer-shop_manager";s:7:"default";s:22:"setting-backend-editor";s:7:"default";s:22:"setting-backend-author";s:7:"default";s:27:"setting-backend-contributor";s:7:"default";s:28:"setting-backend-shop_manager";s:7:"default";s:23:"setting-frontend-editor";s:7:"default";s:23:"setting-frontend-author";s:7:"default";s:28:"setting-frontend-contributor";s:7:"default";s:29:"setting-frontend-shop_manager";s:7:"default";s:16:"setting-fontello";s:0:"";s:4:"skin";s:92:"https://themify.me/demo/themes/ultra-spa/wp-content/themes/themify-ultra/skins/spa/style.css";s:27:"setting-search_exclude_post";s:0:"";s:31:"setting-search_settings_exclude";s:0:"";s:30:"setting-search_exclude_product";s:0:"";s:23:"setting-exclude_img_rss";s:0:"";s:20:"setting-excerpt_more";s:0:"";s:35:"setting-default_display_date_inline";s:0:"";s:27:"setting-auto_featured_image";s:0:"";s:40:"setting-default_page_display_date_inline";s:0:"";s:22:"setting-comments_posts";s:0:"";s:23:"setting-post_author_box";s:0:"";s:24:"setting-post_nav_disable";s:0:"";s:25:"setting-post_nav_same_cat";s:0:"";s:22:"setting-comments_pages";s:0:"";s:29:"setting-portfolio_nav_disable";s:0:"";s:30:"setting-portfolio_nav_same_cat";s:0:"";s:29:"setting-hide_shop_breadcrumbs";s:0:"";s:23:"setting-hide_shop_count";s:0:"";s:25:"setting-hide_shop_sorting";s:0:"";s:23:"setting-product_reviews";s:0:"";s:24:"setting-related_products";s:0:"";s:33:"setting-disable_responsive_design";s:0:"";s:31:"setting-lightbox_content_images";s:0:"";s:18:"setting-cache_gzip";s:0:"";s:29:"setting-fixed_header_disabled";s:0:"";s:26:"setting-full_height_header";s:0:"";s:25:"setting-exclude_site_logo";s:0:"";s:30:"setting-exclude_header_widgets";s:0:"";s:31:"setting-exclude_menu_navigation";s:0:"";s:25:"setting-exclude_cart_icon";s:0:"";s:30:"setting-exclude_footer_widgets";s:0:"";s:28:"setting-exclude_footer_texts";s:0:"";s:27:"setting-exclude_footer_back";s:0:"";s:38:"setting-header_color_animation_enabled";s:0:"";s:38:"setting-footer_color_animation_enabled";s:0:"";s:40:"setting-relationship_taxonomy_hide_image";s:0:"";s:20:"setting-autoinfinite";s:0:"";s:29:"setting-footer_text_left_hide";s:0:"";s:30:"setting-footer_text_right_hide";s:0:"";s:38:"setting-page_builder_disable_shortcuts";s:0:"";s:34:"setting-page_builder_exc_accordion";s:0:"";s:28:"setting-page_builder_exc_box";s:0:"";s:32:"setting-page_builder_exc_buttons";s:0:"";s:32:"setting-page_builder_exc_callout";s:0:"";s:32:"setting-page_builder_exc_contact";s:0:"";s:32:"setting-page_builder_exc_divider";s:0:"";s:38:"setting-page_builder_exc_fancy-heading";s:0:"";s:32:"setting-page_builder_exc_feature";s:0:"";s:32:"setting-page_builder_exc_gallery";s:0:"";s:34:"setting-page_builder_exc_highlight";s:0:"";s:29:"setting-page_builder_exc_icon";s:0:"";s:30:"setting-page_builder_exc_image";s:0:"";s:36:"setting-page_builder_exc_layout-part";s:0:"";s:28:"setting-page_builder_exc_map";s:0:"";s:33:"setting-page_builder_exc_maps-pro";s:0:"";s:29:"setting-page_builder_exc_menu";s:0:"";s:35:"setting-page_builder_exc_plain-text";s:0:"";s:34:"setting-page_builder_exc_portfolio";s:0:"";s:29:"setting-page_builder_exc_post";s:0:"";s:38:"setting-page_builder_exc_pricing-table";s:0:"";s:43:"setting-page_builder_exc_product-categories";s:0:"";s:33:"setting-page_builder_exc_products";s:0:"";s:37:"setting-page_builder_exc_service-menu";s:0:"";s:31:"setting-page_builder_exc_slider";s:0:"";s:28:"setting-page_builder_exc_tab";s:0:"";s:43:"setting-page_builder_exc_testimonial-slider";s:0:"";s:36:"setting-page_builder_exc_testimonial";s:0:"";s:29:"setting-page_builder_exc_text";s:0:"";s:30:"setting-page_builder_exc_video";s:0:"";s:31:"setting-page_builder_exc_widget";s:0:"";s:35:"setting-page_builder_exc_widgetized";s:0:"";s:19:"setting-post_filter";s:3:"yes";s:29:"setting-portfolio_post_filter";s:3:"yes";s:23:"setting-products_layout";s:5:"grid4";s:40:"setting-product_archive_hide_cart_button";s:0:"";s:36:"setting-hide_shop_single_breadcrumbs";s:0:"";}<?php $themify_data = unserialize( ob_get_clean() );

	// fix the weird way "skin" is saved
	if( isset( $themify_data['skin'] ) ) {
		$parsed_skin = parse_url( $themify_data['skin'], PHP_URL_PATH );
		$basedir_skin = basename( dirname( $parsed_skin ) );
		$themify_data['skin'] = trailingslashit( get_template_directory_uri() ) . 'skins/' . $basedir_skin . '/style.css';
	}

	themify_set_data( $themify_data );
	
}
themify_do_demo_import();